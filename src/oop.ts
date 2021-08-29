import { ModifierList } from "./ast";
import { GeneratedBlock, GeneratedExpression } from "./generator";
import { BinaryOperator, Modifier, transformOperatorResult, UnaryOperator } from "./operators";
import { Returned, Runtime } from "./runtime";
import { BaseTokenizer, TextRange, Token } from "./tokenizer";
import { DummySplashType, SplashClass, SplashFunctionType, SplashOptionalType, SplashParameterizedType, SplashPrimitive, SplashString, SplashType } from "./types";
import { NativeMethods } from "./native";
import { Parser } from "./parser";
import { Processor } from "./processor";
import { TextLocation } from "./env";

export abstract class TypeToken {

    optional = false

    constructor() {

    }

    abstract get range(): TextRange

    abstract toString(): string

}

export abstract class SingleTypeToken extends TypeToken {
    constructor(public range: TextRange) {
        super()
    }
    
}

export class BasicTypeToken extends SingleTypeToken {

    static void = new BasicTypeToken(TextRange.end,Token.dummy('void'),[])
    static object = new BasicTypeToken(TextRange.end,Token.dummy('object'),[])

    constructor(public range: TextRange, public base: Token, public typeParams: TypeToken[]) {
        super(range)
    }

    toString() {
        return this.base.value + (this.typeParams.length == 0 ? '' : '<' + this.typeParams.join(',') + '>') + (this.optional ? '?' : '')
    }
}

export class FunctionTypeToken extends SingleTypeToken {
    constructor(public range: TextRange, public params: TypeToken[], public returnType: TypeToken, optional: boolean) {
        super(range)
        this.optional = optional
    }

    toString() {
        return '(' + this.params.join(', ') + ')=>' + this.returnType.toString()
    }
}

export class ComboTypeToken extends TypeToken {
    
    constructor(public options: TypeToken[]) {
        super()
    }

    get range(): TextRange {
        return TextRange.between(this.options[0].range,this.options[this.options.length-1].range)
    }
    toString(): string {
        return this.options.join(' | ')
    }
}




export interface Member {
    name: string
    docs?: string
    decl?: TextLocation
    type: SplashType
    isStatic: boolean
}

export abstract class ClassExecutable implements Member {

    docs?: string
    constructor(public params: Parameter[], public modifiers: ModifierList, public decl?: TextLocation) {

    }
    abstract name: string;
    abstract type: SplashType;
    abstract get isStatic(): boolean

    abstract invoke(runtime: Runtime, inType: SplashType, thisArg?: Value, ...params: Value[]): Value

}

export class Method extends ClassExecutable {
    
    body?: GeneratedBlock
    type: SplashType

    constructor(public name: string, public retType: SplashType, params: Parameter[], modifiers: ModifierList, decl?: TextLocation) {
        super(params,modifiers,decl)
        if (this.modifiers.has(Modifier.get)) {
            this.type = this.retType
        } else {
            this.type = new SplashFunctionType(this.retType, this.params.map(p=>p.type))
        }
    }

    resolveFunctionType(ownerType: SplashType) {
        if (this.type instanceof SplashFunctionType) {
            return new SplashFunctionType(this.retType.resolve(ownerType),this.params.map(p=>p.resolve(ownerType).type))
        }
        return this.retType.resolve(ownerType)
    }

    invoke(runtime: Runtime, inType: SplashType, thisArg?: Value, ...params: Value[]): Value {
        let r: Runtime
        if (this.modifiers.has(Modifier.static)) {
            r = runtime.inTypeStatic(inType)
        } else if (thisArg) {
            r = runtime.inTypeInstance(thisArg)
        } else {
            return Value.null
        }
        if (this.body) {
            Parameter.initParams(r,this.params,params)
            try {
                this.body.run(r)
            } catch (e) {
                if (e instanceof Returned) {
                    return e.value
                }
            }
            return Value.void
        } else {
            return NativeMethods.invoke(r, inType, this.name, params, thisArg)
        }
    }

    get isStatic() {
        return this.modifiers.has(Modifier.static)
    }

    toString() {
        return this.modifiers.toString() + this.retType.toString() + ' ' + this.name + '(' + this.params.join(', ') + ')'
    }

}

export class Parameter {
    defValue?: GeneratedExpression
    constructor(public name: string, public type: SplashType, public hasDefValue?: boolean, public vararg?: boolean) {

    }

    resolve(ownerType: SplashType) {
        let p = new Parameter(this.name, this.type.resolve(ownerType), this.hasDefValue, this.vararg)
        p.defValue = this.defValue
        return p
    }

    toString() {
        return this.type.toString() + ' ' + this.name
    }

    static getParamAt(index: number, params: Parameter[]) {
        if (params.length == 0) return
        if (index < params.length) {
            return params[index]
        }
        if (params[params.length - 1].vararg) {
            return params[params.length - 1]
        }
    }

    static initParams(runtime: Runtime, params: Parameter[], values: Value[]) {
        for (let i = 0; i < params.length; i++) {
            let p = params[i]
            if (p.vararg) {
                let vals = values.slice(i,values.length)
                runtime.setVariable(p.name, new Value(p.type, vals))
            } else if (i < values.length) {
                runtime.setVariable(p.name, values[i])
            } else if (p.type instanceof SplashOptionalType) {
                runtime.setVariable(p.name, new Value(p.type, null))
            } else {
                let val = p.defValue?.evaluate(runtime) || Value.null
                runtime.setVariable(p.name, val)
            }
        }
    }

    static allParamsMatch(params: Parameter[], types: SplashType[]): boolean {
        let matchCount = 0;
        for (let i = 0; i < params.length; i++) {
            let p = params[i]
            if (i < types.length) {
                if (p.vararg) {
                    let rest = types.slice(i)
                    return rest.every(r=>r.canAssignTo((p.type as SplashParameterizedType).params[0]))
                } else {
                    if (!types[i].canAssignTo(p.type)) {
                        return false
                    }
                }
                matchCount++
            } else if (!(p.type instanceof SplashOptionalType) && !p.hasDefValue) {
                return false
            }
            
        }
        return matchCount == types.length;
    }

    static readFromString(str: string, proc: Processor, inType?: SplashType) {
        let parser = new Parser('unknown',new BaseTokenizer(str))
        let p = parser.parseParameter()
        if (!p) throw 'Cannot create parameter from ' + str
        p.process(proc)
        return p.generate()
    }
}

export class CtorParameter extends Parameter {
    constructor(name: string, type: SplashType, public assignToField: boolean, hasDefValue?: boolean, public vararg?: boolean) {
        super(name,type,hasDefValue,vararg)
    }
}

export class Constructor extends ClassExecutable {

    body?: GeneratedBlock
    type: SplashType
    name: string = 'constructor'
    constructor(type: SplashType, params: CtorParameter[], modifiers: ModifierList, decl: TextLocation) {
        super(params,modifiers,decl)
        this.type = new SplashFunctionType(type, this.params.map(p=>p.type))
    }

    invoke(runtime: Runtime, inType: SplashType, thisArg?: Value, ...params: Value[]): Value {
        let val = new Value(inType,{})
        let r = runtime.inTypeInstance(val)
        for (let m of inType.members) {
            if (m instanceof Field) {
                val.set(r,m.name,m.defaultValue(r))
            }
        }
        
        Parameter.initParams(r, this.params, params)
        for (let i = 0; i < this.params.length; i++) {
            let cp = this.params[i]
            let v = r.getVariable(cp.name)
            if (cp instanceof CtorParameter && cp.assignToField) {
                val.set(r,cp.name,v)
            }
        }
        this.body?.run(r)
        return val
    }

    get isStatic() {
        return true
    }
    
    toString() {
        return '(constructor) ' + this.modifiers.toString() + this.name + '(' + this.params.join(',') + ')'
    }

}

export class Field implements Member {
    docs?: string
    constructor(public name: string, public modifiers: ModifierList, public type: SplashType, public decl: TextLocation, public init?: GeneratedExpression) {
        
    }
    
    defaultValue(runtime: Runtime): Value {
        return this.init ? this.init.evaluate(runtime) : this.type.defaultValue
    }

    get isStatic() {
        return this.modifiers.has(Modifier.static)
    }

    toString() {
        return '(field) ' + this.name + ': ' + this.type.toString()
    }

}

export class Value {

    static dummy = new Value(SplashClass.object,{})
    static void = new Value(DummySplashType.void,undefined)
    static null = new Value(DummySplashType.null,null)

    uid: number

    constructor(public type: SplashType, public inner: any) {
        this.uid = Math.round(Math.random() * 100000)
    }

    invokeMethod(runtime: Runtime, name: string, ...params: Value[]): Value {
        let method = this.type.getValidMethod(name,...params.map(p=>p.type))
        if (!method) {
            console.log('could not find method',name,params,'in',this)
            return Value.null
        }
        return method.invoke(runtime,this.type,this,...params)
    }

    invoke(runtime: Runtime, ...params: Value[]) {
        let invoker = this.type.methods.filter(m=>m.modifiers.has(Modifier.invoker))
            .filter(m=>Parameter.allParamsMatch(m.params, params.map(v=>v.type)))
        
        return invoker[0].invoke(runtime,this.type,this,...params)
    }

    getAccessor() {
        return this.type.methods.filter(m=>m.modifiers.has(Modifier.accessor))
            .filter(m=>m.params.length == 1 && m.params[0].type == SplashString.instance)
            [0]
    }

    getAssigner(type: SplashType) {
        return this.type.methods.filter(m=>m.modifiers.has(Modifier.assigner))
            .filter(m=>m.params.length == 2 
                && m.params[0].type == SplashString.instance
                && type.canAssignTo(m.params[1].type))
            [0]
    }

    get(runtime: Runtime, field: string): Value {
        let acc = this.getAccessor()
        if (acc) {
            return acc.invoke(runtime, this.type, this, new Value(SplashString.instance, field))
        }
        
        let getterMethod = this.type.getMethods(field)
        if (getterMethod.length > 0) {
            return getterMethod[0].invoke(runtime, this.type, this)
        }

        if (this.isPrimitive) {
            return Value.null
        }
        
        return this.inner[field]
    }

    set(runtime: Runtime, field: string, value: Value) {
        let ass = this.getAssigner(value.type)
        if (ass) {
            ass.invoke(runtime, this.type, this, new Value(SplashString.instance, field), value)
        }
        if (!this.isPrimitive) {
            this.inner[field] = value
        }
    }

    getIndex(runtime: Runtime, index: Value) {
        let indexer = this.type.getIndexGetter(index.type)
        if (indexer) {
            return indexer.invoke(runtime, this.type, this, index)
        }
    }

    setIndex(runtime: Runtime, index: Value, value: Value) {
        let indexer = this.type.getIndexSetter(index.type,value.type)
        if (indexer) {
            indexer.invoke(runtime, this.type, this, index, value)
        }
    }

    invokeBinOperator(runtime: Runtime, op: BinaryOperator, other: Value): Value {
        let method = this.type.getBinaryOperation(op,other.type)
        if (!method) {
            method = other.type.getBinaryOperation(op,this.type)
            if (method?.modifiers.has(Modifier.bidir)) {
                let res = method.invoke(runtime, this.type, other, this)
                return transformOperatorResult(res, method.name, op)
            }
            console.log('did not find bin operator',this,op,other)
            return Value.dummy
        }
        let res = method.invoke(runtime, this.type, this, other)
        return transformOperatorResult(res, method.name, op)
    }

    invokeUnaryOperator(runtime: Runtime, op: UnaryOperator): Value {
        let method = this.type.getUnaryOperation(op)
        return method?.invoke(runtime, this.type, this) || Value.dummy
    }

    invokeIterator(runtime: Runtime): Value {
        let method = this.type.getIterator()
        return method?.invoke(runtime, this.type, this) || Value.dummy
    }

    toString(runtime: Runtime): string {
        if (this.isPrimitive) {
            if (this.isNull) {
                return 'null'
            }
            return this.inner.toString()
        }
        return this.invokeMethod(runtime, 'toString').inner
    }

    toBoolean(runtime: Runtime): boolean {
        if (this.isPrimitive) {
            return this.inner ? true : false
        }
        return this.invokeMethod(runtime, 'toBoolean').inner
    }

    get isNull() {
        return this.type == DummySplashType.null || this.inner == null
    }

    get isVoid() {
        return this.type == DummySplashType.void
    }

    get isPrimitive() {
        return this.type instanceof SplashPrimitive || this.isNull || this.isVoid
    }
}
