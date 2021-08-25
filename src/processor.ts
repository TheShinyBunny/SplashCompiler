import { ModifierList, ParameterNode, RootNode, FunctionNode, ASTNode, TypeParameterNode } from "./ast";
import { SplashFunction, SplashScript } from "./generator";
import { BasicTypeToken, ComboTypeToken, FunctionTypeToken, Method, SingleTypeToken, TypeToken } from "./oop";
import { BuiltinTypes, DummySplashType, SelfSplashType, SplashClass, SplashComboType, SplashFunctionType, SplashInt, SplashOptionalType, SplashParameterizedType, SplashPrimitive, SplashString, SplashType, TypeParameter } from "./types";
import { BaseTokenizer, Position, TextRange, Token } from "./tokenizer";
import { Completion, CompletionType, PartialCompletion, SplashModule, TextLocation, TokenInfo } from "./env";
import { Parser } from "./parser";
import { Diagnostic, DiagnosticType } from ".";


export class Processor {

    variables: VariableFrame[] = [{}]
    types: SplashType[] = []
    rawFunctions: FunctionNode[] = []
    functions: SplashFunction[] = []
    currentFile?: string
    currentClass?: SplashType
    currentFunction?: SplashFunction | Method
    diagnostics: Diagnostic[] = []
    cursor?: Position
    completionItems: Completion[] = []
    tokenInfos: TokenInfo[] = []
    hasReturn = false
    silent = false
    inInstanceContext = false


    constructor() {
        this.types.push(...Object.values(BuiltinTypes))
    }

    import(module: SplashModule) {
        for (let s of module.scripts) {
            this.importScript(s)
        }
    }

    importScript(script: SplashScript) {
        this.types.push(...script.classes)
        this.functions.push(...script.functions)
    }
    

    process(ast: RootNode) {
        this.currentFile = ast.file
        ast.process(this)
    }
    
    suggest(range: TextRange, generator: ()=>PartialCompletion[]) {
        if (this.cursor && TextRange.contains(range,this.cursor)) {
            this.completionItems.push(...generator().map(c=>({...c, range})))
        }
    }

    addInfo(info: TokenInfo) {
        this.tokenInfos.push(info)
    }

    error(range: TextRange, msg: string) {
        if (!this.silent) {
            console.log("Validation error in " + this.currentFile + " at " + TextRange.toString(range) + ": " + msg)
            this.diagnostics.push({file: this.currentFile || '',range, message: msg, type: DiagnosticType.error})
        } else {
            console.log('skipped error, processor is silent (',msg,')')
        }
    }
    
    location(range: TextRange): TextLocation {
        return {range, file: this.currentFile || 'unknown'}
    }

    push() {
        this.variables.push({})
    }

    pop() {
        this.variables.pop()
    }

    addVariable(name: Token, type: SplashType) {
        if (this.variables.length > 0) {
            this.variables[this.variables.length-1][name.value] = new Variable(name, type)
        }
    }

    declareVariable(name: Token, type: SplashType) {
        if (this.getVariable(name.value)) {
            this.error(name.range, "Duplicate variable " + name.value + "!")
        } else {
            this.addVariable(name, type)
        }
    }

    getTypeByName(name: string) {
        let tp = this.getTypeParam(name)
        if (tp) return tp
        return this.types.find(t=>t.name == name)
    }

    validateType(token: TypeToken) {
        this.suggestTypes(token)
        let res = this.resolveType(token)
        if (token.toString() != 'null' && res == DummySplashType.null) {
            this.error(token.range,"Unknown type " + token)
        } else {
            this.addInfosToType(token,res)
        }
    }

    addInfosToType(token: TypeToken, type: SplashType) {
        if (type instanceof SplashClass || type instanceof SplashPrimitive || type instanceof DummySplashType || type instanceof TypeParameter) {
            this.addInfo({range: token.range, detail: type.toString(),declaration: type.declaration})
        } else if (type instanceof SplashComboType && token instanceof ComboTypeToken) {
            type.types.forEach((t,i)=>this.addInfosToType(token.options[i],t))
        } else if (type instanceof SplashOptionalType) {
            this.addInfosToType(token,type.inner)
        } else if (type instanceof SelfSplashType) {
            this.addInfosToType(token,type.base)
        } else if (type instanceof SplashParameterizedType) {
            let tok = token as BasicTypeToken
            this.addInfosToType(tok,type.base)
            type.params.forEach((p,i)=>this.addInfosToType(tok.typeParams[i],p))
        }
    }

    suggestTypes(token: TypeToken) {
        if (token instanceof BasicTypeToken) {
            this.suggest(token.range,()=>this.getTypeParams().map(tp=>({value: tp.name, type: CompletionType.typeParam})))
            this.suggest(token.range,()=>this.types.map(t=>({value: t.name, type: CompletionType.class})))
            token.typeParams.forEach(p=>this.suggestTypes(p))
        } else if (token instanceof FunctionTypeToken) {
            this.suggestTypes(token.returnType)
            token.params.forEach(p=>this.suggestTypes(p))
        } else if (token instanceof ComboTypeToken) {
            token.options.forEach(o=>this.suggestTypes(o))
        }
    }

    getFunctions(): BaseFunction[] {
        let funcs: BaseFunction[] = []
        if (this.currentClass) {
            funcs.push(...this.currentClass.methods.map(m=>({name: m.name, type: m.resolveFunctionType(this.currentClass || SplashClass.object), docs: m.docs, decl: m.decl})))
        }
        funcs.push(...this.functions.map(f=>({name: f.name, type: f.toFunctionType(), docs: f.docs, decl: f.decl})))
        funcs.push(...this.rawFunctions.map(rf=>({name: rf.name.value, type: rf.toFunctionType(this), docs: rf.docs, decl: rf.label})))
        console.log('functions:',funcs)
        return funcs
    }

    resolveType(token: TypeToken): SplashType {
        if (token instanceof SingleTypeToken) {
            return this.resolveTypeFromSingle(token) || DummySplashType.null
        } else if (token instanceof ComboTypeToken) {
            return new SplashComboType(token.options.map(t=>this.resolveType(t)))
        }
        return DummySplashType.null
    }

    resolveTypeFromSingle(token: SingleTypeToken): SplashType {
        let res: SplashType | undefined = DummySplashType.null
        if (token instanceof BasicTypeToken) {
            res = this.getTypeByName(token.base.value)
            if (res) {
                if (token.typeParams.length > 0) {
                    let hasInvalid = false
                    let params = token.typeParams.map(p=>{
                        let rt = this.resolveType(p)
                        if (!rt) hasInvalid = true
                        return rt
                    })
                    if (hasInvalid) return DummySplashType.null
                    res = new SplashParameterizedType(res,params)
                }
            } else if (token.base.value == 'this' && this.currentClass) {
                res = new SelfSplashType(this.currentClass)
            }
        } else if (token instanceof FunctionTypeToken) {
            res = new SplashFunctionType(this.resolveType(token.returnType), token.params.map(p=>this.resolveType(p)))
        }
        if (!res) {
            return DummySplashType.null
        }
        return token.optional ? SplashOptionalType.of(res) : res
    }

    resolveTypeFromString(str: string) {
        let token = new Parser('unknown',new BaseTokenizer(str)).parseTypeToken(true)
        if (!token) return
        return this.resolveType(token)
    }


    getVariable(name: string) {
        for (let i = this.variables.length - 1; i >= 0; i--) {
            let frame = this.variables[i]
            if (frame[name]) {
                return frame[name]
            }
        }
    }

    getTypeParams() {
        if (this.currentClass) {
            return this.currentClass.typeParams
        }
        //todo: add function type params
        return []
    }

    getTypeParam(name: string) {
        if (this.currentClass) {
            let tp = this.currentClass.typeParams.find(t=>t.name == name)
            if (tp) return tp
        }
        //todo: add function type params
    }
}

export type VariableFrame = {[id: string]: Variable}

export class Variable {
    constructor(public name: Token, public type: SplashType) {

    }
}

export interface BaseFunction {
    name: string
    type: SplashType
    docs?: string
    decl?: TextLocation
}