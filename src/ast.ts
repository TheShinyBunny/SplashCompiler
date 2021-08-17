import { GenArrayCreation, GenAssignableExpression, GenAssignment, GenCall, GenCallAccess, GenClassDecl, GenConstExpression, Generated, GeneratedBinary, GeneratedBlock, GeneratedExpression, GeneratedLiteral, GeneratedReturn, GeneratedStatement, GeneratedUnary, GenFieldAccess, SplashFunction, GenIfStatement, GenStringLiteral, GenVarAccess, GenVarDeclaration, SplashScript, GenIndexAccess, GeneratedRepeat, GeneratedFor } from "./generator";
import { ExpressionSegment, StringToken, TextRange, Token, TokenType } from "./tokenizer";
import { Parser } from "./parser";
import { Constructor, CtorParameter, Field, Member, Method, Parameter, TypeToken, Value } from "./oop";
import { Processor } from "./processor";
import { AssignmentOperator, BinaryOperator, getActualOpReturnType, getOpMethodName, isBidirectional, Modifier, UnaryOperator } from "./operators";
import { BuiltinTypes, DummySplashType, SplashArray, SplashBoolean, SplashClass, SplashClassType, SplashComboType, SplashFloat, SplashFunctionType, SplashInt, SplashOptionalType, SplashParameterizedType, SplashString, SplashType, TypeParameter } from "./types";


export abstract class ASTNode {
    constructor(public id: string) {

    }
}

export class RootNode extends ASTNode {

    classes: ClassDeclaration[] = []
    main?: MainBlock
    functions: FunctionNode[] = []

    constructor(public file: string) {
        super("root")
    }

    add(node: ASTNode) {
        if (node instanceof ClassDeclaration) {
            this.classes.push(node)
        } else if (node instanceof FunctionNode) {
            this.functions.push(node)
        } else if (node instanceof MainBlock) {
            this.main = node
        }
    }

    index(proc: Processor) {
        for (let c of this.classes) {
            c.index(proc)
        }
        for (let f of this.functions) {
            f.index(proc)
        }
    }

    process(proc: Processor) {
        
        for (let c of this.classes) {
            c.process(proc)
        }
        for (let f of this.functions) {
            f.process(proc)
        }

        if (this.main) {
            this.main.process(proc)
        }
    }

    generate(proc: Processor): SplashScript {
        let script = new SplashScript(this.file)
        script.main = this.main?.generate(proc)
        script.classes.push(...this.classes.map(c=>c.generate(proc)))
        script.functions.push(...this.functions.map(f=>f.generate(proc)))
        return script
    }
}

export abstract class Statement extends ASTNode {
    constructor(id: string, public label: TextRange) {
        super(id)
    }

    abstract process(proc: Processor): void

    abstract generate(proc: Processor): GeneratedStatement
}

export class CodeBlock extends Statement {
    statements: Statement[] = []

    process(proc: Processor) {
        proc.push()
        for (let s of this.statements) {
            s.process(proc)
        }
        proc.pop()
    }

    generate(proc: Processor) {
        return new GeneratedBlock(this.statements.map(s=>s.generate(proc)))
    }
}

export class MainBlock extends CodeBlock {
    constructor(label: TextRange, statements: Statement[]) {
        super("main",label)
        this.statements = statements
    }
}

export class VarDeclaration extends Statement {
    
    constructor(label: TextRange, public name: Token, public init?: Expression) {
        super("var_declaration",label)
    }

    generate(proc: Processor): GenVarDeclaration {
        return new GenVarDeclaration(this.name.value,this.init?.generate(proc))
    }
    process(proc: Processor): void {
        proc.declareVariable(this.name, this.init ? this.init.getResultType(proc) : SplashClass.object)
    }

    
}

export class IfStatement extends Statement {
    
    constructor(label: TextRange, public expr: Expression, public then: Statement, public orElse?: ElseStatement) {
        super("if",label)
    }

    process(proc: Processor): void {
        this.expr.getResultType(proc)
        proc.push()
        this.then.process(proc)
        proc.pop()
        this.orElse?.process(proc)
    }
    generate(proc: Processor): GeneratedStatement {
        return new GenIfStatement(this.expr.generate(proc),this.then.generate(proc),this.orElse?.generate(proc))
    }
}

export class ElseStatement extends Statement {
    
    constructor(label: TextRange, public code: Statement) {
        super("else",label)
    }

    process(proc: Processor): void {
        proc.push()
        this.code.process(proc)
        proc.pop()
    }
    generate(proc: Processor): GeneratedStatement {
        return this.code.generate(proc)
    }
}

export abstract class Expression extends ASTNode {

    constructor(id: string, public range: TextRange) {
        super(id)
    }

    abstract getResultType(proc: Processor): SplashType

    abstract generate(proc: Processor): GeneratedExpression
}

export class ExpressionList {

    constructor(public values: Expression[], public range: TextRange) {

    }

    canApplyTo(proc: Processor, parameters: Parameter[], reportErrors: boolean) {
        if (!reportErrors) {
            return Parameter.allParamsMatch(parameters, this.values.map(v=>v.getResultType(proc)))
        }
        let successful = true
        let matchCount = 0;
        for (let i = 0; i < parameters.length; i++) {
            let p = parameters[i]
            if (i < this.values.length) {
                if (p.vararg) {
                    let rest = this.values.slice(i)
                    let allMatch = rest.every(r=>r.getResultType(proc).canAssignTo((p.type as SplashParameterizedType).params[0]))
                    if (!allMatch) {
                        successful = false
                        proc.error(TextRange.between(rest[0].range,rest[rest.length-1].range),'Vararg values types do not all match ' + p.type)
                    }
                } else {
                    let type = this.values[i].getResultType(proc);
                    if (!type.canAssignTo(p.type)) {
                        successful = false
                        proc.error(this.values[i].range,'Argument ' + type + ' cannot be assigned to parameter ' + p.type)
                    }
                }
                matchCount++
            } else if (!(p.type instanceof SplashOptionalType) && !p.hasDefValue) {
                successful = false
                proc.error(this.range,' Missing required parameter ' + p.type)
            }
        }
        if (!successful) return false
        let match = matchCount == this.values.length;
        if (!match) {
            proc.error(this.range, 'Mismatched argument count provided, expected ' + parameters.length)
        }
        return match
    }

    generate(proc: Processor) {
        return this.values.map(v=>v.generate(proc))
    }

}

export class BinaryExpression extends Expression {
    
    constructor(public left: Expression, public op: Token, public right: Expression) {
        super("binary_expression",TextRange.between(left.range, right.range))
    }

    getResultType(proc: Processor): SplashType {
        let leftType = this.left.getResultType(proc)
        let rightType = this.right.getResultType(proc)
        let binop = leftType.getBinaryOperation(this.op.value as BinaryOperator, rightType)
        if (!binop && isBidirectional(this.op.value as BinaryOperator)) {
            binop = rightType.getBinaryOperation(this.op.value as BinaryOperator, leftType)
        }
        if (binop) {
            return getActualOpReturnType(this.op.value as BinaryOperator,binop.retType)
        }
        proc.error(this.op.range, "Operator " + this.op.value + " cannot be applied to " + leftType + ' and ' + rightType)
        return SplashClass.object
    }

    generate(proc: Processor): GeneratedExpression {
        return new GeneratedBinary(this.left.generate(proc), this.op.value as BinaryOperator, this.right.generate(proc))
    }
}

export class UnaryExpression extends Expression {
    
    constructor(public op: Token, public expr: Expression) {
        super("unary_expression", TextRange.between(op.range, expr.range))
    }

    getResultType(proc: Processor) {
        let type = this.expr.getResultType(proc)
        let uop = type.getUnaryOperation(this.op.value as UnaryOperator)
        if (uop) {
            return uop.retType
        }
        proc.error(this.op.range, "Unary operator " + this.op.value + " cannot be applied to " + type)
        return SplashClass.object
    }
    generate(proc: Processor): GeneratedExpression {
        return new GeneratedUnary(this.expr.generate(proc),this.op.value as UnaryOperator)
    }
}

export class LiteralExpression extends Expression {
    
    constructor(public token: Token) {
        super('literal_expression', token.range)
    }

    getResultType(proc: Processor): SplashType {
        switch (this.token.type) {
            case TokenType.int:
                return SplashInt.instance
            case TokenType.float:
                return SplashFloat.instance
            case TokenType.string:
                return SplashString.instance
        }
        if (this.token.value == 'true' || this.token.value == 'false') {
            return SplashBoolean.instance
        }
        return SplashClass.object
    }
    generate(proc: Processor): GeneratedExpression {
        return new GeneratedLiteral(this.token.type, this.token.value)
    }
}

export class StringExpression extends Expression {
    
    constructor(public range: TextRange, public nodes: Expression[]) {
        super('string_literal',range)
    }

    getResultType(proc: Processor): SplashType {
        for (let s of this.nodes) {
            s.getResultType(proc)
        }
        return SplashString.instance
    }
    generate(proc: Processor): GeneratedExpression {
        return new GenStringLiteral(this.nodes.map(n=>n.generate(proc)))
    }
}

export class InvalidExpression extends Expression {
    
    constructor() {
        super('invalid_expression', TextRange.end)
    }

    getResultType(proc: Processor): SplashType {
        return SplashClass.object
    }
    generate(proc: Processor): GeneratedExpression {
        return GeneratedLiteral.invalid
    }
}

export class ArrayExpression extends Expression {
    
    constructor(public values: ExpressionList) {
        super('array_expression', values.range)
    }

    getResultType(proc: Processor): SplashType {
        let valueType: SplashType = SplashClass.object
        for (let v of this.values.values) {
            let t = v.getResultType(proc)
            if (valueType == SplashClass.object) {
                valueType = t
            } else if (valueType != t) {
                if (valueType instanceof SplashComboType) {
                    valueType = new SplashComboType([t,...valueType.types])
                } else {
                    valueType = new SplashComboType([valueType,t])
                }
            }
        }
        return SplashArray.of(valueType)
    }
    generate(proc: Processor): GeneratedExpression {
        return new GenArrayCreation(this.values.generate(proc))
    }
    
}

export class Assignment extends Statement {
    
    constructor(public variable: AssignableExpression, public op: Token, public expression: Expression) {
        super("assignment",op.range)
    }

    process(proc: Processor): void {
        let v = this.variable.getResultType(proc)
        let val = this.expression.getResultType(proc)
        if (this.op.value == '=') {
            if (!val.canAssignTo(v)) {
                proc.error(this.expression.range, "Expression of type " + val + " cannot be assigned to " + v)
            }
        } else {
            let binop = this.op.value.substring(0,this.op.value.length - 1) as BinaryOperator
            let opm = v.getBinaryOperation(binop, val)
            if (opm) {
                if (!opm.retType.canAssignTo(v)) {
                    proc.error(this.op.range, "Expression of type " + val + " cannot be assigned by applying operator " + this.op.value + " with " + v + " (result is " + opm.retType + ")")
                }
            } else {
                proc.error(this.op.range, "Expression of type " + val + " cannot be applied to " + v + " with " + this.op.value)
            }
        }
    }
    generate(proc: Processor): GeneratedStatement {
        return new GenAssignment(this.variable.generate(proc), this.op.value as AssignmentOperator, this.expression.generate(proc))
    }
}

export abstract class AssignableExpression extends Expression {
    abstract generate(proc: Processor): GenAssignableExpression
}

export class FieldAccess extends AssignableExpression {
    
    constructor(public field: Token, public parent: Expression) {
        super("field_access",TextRange.between(parent.range, field.range))
    }

    getResultType(proc: Processor): SplashType {
        let parentType = this.parent.getResultType(proc)
        let t = parentType.getMemberTypes(proc,this.field.value)
        if (t.length == 1) {
            return t[0]
        }
        if (t.length > 0) {
            return new SplashComboType(t)
        }
        proc.error(this.field.range, "Unknown member " + this.field.value + " in type " + parentType)
        return SplashClass.object
    }
    generate(proc: Processor): GenAssignableExpression {
        return new GenFieldAccess(this.parent.generate(proc),this.field.value)
    }
}

export class VariableAccess extends AssignableExpression {
    
    constructor(public name: Token) {
        super("variable_access", name.range)
    }

    getResultType(proc: Processor): SplashType {
        let v = proc.getVariable(this.name.value);
        let func = proc.getFunctionType(this.name.value)
        if (v) {
            if (func) {
                return SplashType.combine([v.type, func])
            }
            return v.type
        } else if (func) {
            return func
        } else {
            let cls = proc.getTypeByName(this.name.value)
            if (cls) return SplashClassType.of(cls)
        }
        console.log('variable ' + this.name.value + ' not found in',proc)
        proc.error(this.name.range,"Unknown variable '" + this.name.value + "'")
        return SplashClass.object
    }
    generate(proc: Processor): GenAssignableExpression {
        return new GenVarAccess(this.name.value)
    }
}

export class CallAccess extends Expression {
    
    constructor(public params: ExpressionList, public parent: Expression) {
        super("call_access", params.range)
    }

    getResultType(proc: Processor): SplashType {
        let res = this.parent.getResultType(proc)
        if (res instanceof SplashFunctionType) {
            if (this.params.canApplyTo(proc,res.params.map(t=>new Parameter('',t)),true)) {
                return res.retType
            }
            return SplashClass.object
        } else if (res instanceof SplashComboType) {
            let errFunc: SplashFunctionType | undefined
            for (let option of res.types) {
                if (option instanceof SplashFunctionType) {
                    if (this.params.canApplyTo(proc,option.params.map(t=>new Parameter('',t)),false)) {
                        return option.retType
                    }
                    errFunc = option
                }
            }
            if (errFunc) {
                this.params.canApplyTo(proc,errFunc.params.map(t=>new Parameter('',t)),true)
                return SplashClass.object
            }
        }
        if (res instanceof SplashClassType) {
            if (!res.constructors.find(c=>this.params.canApplyTo(proc,c.params,false))) {
                proc.error(this.range,"No constructor of " + res + " found taking these parameters")
            }
            return res.type
        }
        let invoker = res.getInvoker(proc,this.params);
        if (invoker) {
            return invoker.retType
        }
        proc.error(this.range,'Cannot invoke variable of type ' + res)
        return SplashClass.object
    }
    generate(proc: Processor): GenCallAccess {
        return new GenCallAccess(this.parent.generate(proc), this.params.generate(proc))
    }
}

export class CallStatement extends Statement {
    
    constructor(public call: CallAccess, range: TextRange) {
        super("call",range)
    }

    process(proc: Processor): void {
        this.call.getResultType(proc)
    }

    generate(proc: Processor): GeneratedStatement {
        return new GenCall(this.call.generate(proc))
    }
}

export class IndexAccess extends AssignableExpression {

    constructor(public index: Expression, public parent: Expression) {
        super('index_access',TextRange.between(parent.range,index.range))
    }
    
    getResultType(proc: Processor): SplashType {
        let parentType = this.parent.getResultType(proc)
        let indexType = this.index.getResultType(proc)
        let indexGetter = parentType.getIndexGetter(indexType)
        if (indexGetter) {
            return indexGetter.retType
        }
        let indexSetter = parentType.getIndexSetter(indexType)
        if (indexSetter) {
            return indexSetter.params[1].type
        }
        proc.error(this.range,'No index getter found on value of type ' + parentType + ' taking ' + indexType)
        return SplashClass.object
    }

    generate(proc: Processor): GenAssignableExpression {
        return new GenIndexAccess(this.index.generate(proc),this.parent.generate(proc))
    }
    
}

export class ModifierList extends ASTNode {

    modifiers: Token[] = []

    constructor(initial?: Modifier[]) {
        super('modifiers')
        if (initial) {
            this.modifiers.push(...initial.map(i=>Token.dummy(Modifier[i])))
        }
    }

    has(mod: Modifier) {
        for (let t of this.modifiers) {
            if (Modifier[mod] == t.value) return true
        }
        return false
    }

    add(parser: Parser, mod: Token) {
        if (this.has(Modifier[mod.value as keyof typeof Modifier])) {
            parser.error(mod, "Duplicate modifier '" + mod.value + "'")
        } else {
            this.modifiers.push(mod)
        }
    }

    assertEmpty(parser: Parser) {
        if (this.modifiers.length > 0) {
            parser.errorRange(TextRange.between(this.modifiers[0].range,this.modifiers[this.modifiers.length - 1].range),"Unexpected modifiers")
        }
    }
    
    assertHasOnly(parser: Parser, ...mods: Modifier[]) {
        for (let m of this.modifiers) {
            if (!mods.includes(Modifier[m.value as keyof typeof Modifier])) {
                parser.error(m, "This modifier is invalid here")
            }
        }
    }

    get(modifier: Modifier): Token {
        return this.modifiers.find(m=>m.value == Modifier[modifier]) || Token.EOF
    }

    getOneOf(...modifiers: Modifier[]): Token | undefined {
        for (let m of modifiers) {
            let t = this.get(m)
            if (t.isValid()) return t
        }
    }

    checkIncompatible(parser: Parser, ...mods: Modifier[]) {
        let found: Token[] = []
        for (let m of mods) {
            let t = this.get(m)
            if (t.isValid()) {
                found.push(t)
            }
        }
        if (found.length > 1) {
            for (let m of found) {
                let others = found.filter(t=>t != m).map(t=>t.value).join(", ")
                parser.error(m,`Modifier ${m.value} is incompatible with ${others}`)
            }
        }
    }
}

export class TypeParameterNode extends ASTNode {

    param: TypeParameter | undefined

    constructor(public base: Token, public extend?: TypeToken) {
        super('type_parameter')
    }

    init(proc: Processor, index: number) {
        this.param = new TypeParameter(this.base.value, index, this.extend ? proc.resolveType(this.extend) : undefined)
    }

    process(proc: Processor) {
        if (this.extend) {
            proc.validateType(this.extend)
        }
    }
    
}

export class ParameterNode extends ASTNode {

    constructor(public name: Token, public type: TypeToken, public defValue?: Expression, public vararg?: boolean) {
        super('parameter')
    }

    process(proc: Processor) {
        proc.validateType(this.type)
        if (this.vararg && this.defValue) {
            proc.error(this.name.range, "A parameter cannot be both vararg and have a default value")
        }
        if (this.defValue) {
            let resolved = proc.resolveType(this.type)
            if (!this.defValue.getResultType(proc).canAssignTo(resolved)) {
                proc.error(this.defValue.range, "This expression cannot be assigned to this parameter's type")
            }
        }
    }

    generate(proc: Processor) {
        let type = proc.resolveType(this.type)
        let def: GeneratedExpression | undefined
        if (type instanceof SplashOptionalType) {
            def = new GenConstExpression(new Value(type,null))
        } else if (this.defValue) {
            def = this.defValue.generate(proc)
        }
        let p = new Parameter(this.name.value,type,def !== undefined,this.vararg)
        p.defValue = def
        return p
    }

}

export class FunctionNode extends ASTNode {
    
    constructor(public label: TextRange, public modifiers: ModifierList, public name: Token, public retType: TypeToken, public params: ParameterNode[], public code?: CodeBlock) {
        super('function')
    }

    index(proc: Processor) {
        proc.rawFunctions.push(this)
    }

    process(proc: Processor) {
        if (this.modifiers.has(Modifier.native)) {
            if (this.code) {
                proc.error(this.code.label, "Native functions may not have a body")
            }
        } else if (!this.code) {
            proc.error(this.label, "Missing function body")
        }
        proc.validateType(this.retType)
        let uniqueNames: string[] = []
        let hasVararg = false
        proc.push()
        for (let p of this.params) {
            p.process(proc)
            if (uniqueNames.includes(p.name.value)) {
                proc.error(p.name.range, "Duplicate parameter '" + p.name.value + "'")
            } else {
                uniqueNames.push(p.name.value)
                proc.addVariable(p.name, proc.resolveType(p.type))
            }
            if (hasVararg) {
                proc.error(p.name.range, "Vararg parameter must be the last parameter")
            } else if (p.vararg) {
                hasVararg = true
            }
        }

        if (this.code) {
            this.code.process(proc)
        }

        proc.pop()
        
    }

    generate(proc: Processor): SplashFunction {
        return new SplashFunction(this.name.value,proc.resolveType(this.retType),this.params.map(p=>p.generate(proc)),this.code?.generate(proc))
    }

    toFunctionType(proc: Processor): SplashFunctionType {
        return new SplashFunctionType(proc.resolveType(this.retType), this.params.map(p=>p.generate(proc).type))
    }

}

export class ReturnStatement extends Statement {
    
    constructor(label: TextRange, public expr: Expression | undefined) {
        super('return',label)
    }

    process(proc: Processor): void {
        if (this.expr) {
            let type = this.expr?.getResultType(proc)
            if (proc.currentFunction) {
                if (!proc.currentFunction.retType.canAssignTo(type)) {
                    proc.error(this.expr?.range, "Expression does not match the function's return type")
                }
            }
        } else if (proc.currentFunction && proc.currentFunction.retType != DummySplashType.void) {
            proc.error(this.label, "Function should return a value")
        }
        proc.hasReturn = true
    }
    generate(proc: Processor): GeneratedStatement {
        return new GeneratedReturn(this.expr?.generate(proc))
    }
}

export abstract class ClassMember extends ASTNode {

    abstract index(proc: Processor, type: SplashType): void

    abstract process(proc: Processor): void

    abstract generate(proc: Processor, cls: SplashType): void
}

export class MethodNode extends ClassMember {
    
    method: Method | undefined
    constructor(public name: Token, public retType: TypeToken, public params: ParameterNode[], public modifiers: ModifierList, public body?: CodeBlock) {
        super('method')
    }

    index(proc: Processor, type: SplashType) {
        if (this.modifiers.has(Modifier.native)) {
            if (this.body) {
                proc.error(this.body.label, "Native methods may not have a body")
            }
        } else if (!this.body) {
            proc.error(this.name.range, "Missing method body")
        }
        if (this.modifiers.has(Modifier.abstract) && this.body) {
            proc.error(this.body.label, "Abstract methods may not have a body")
        }
        if (this.modifiers.has(Modifier.accessor)) {
            if (this.params.length != 1 || proc.resolveType(this.params[0].type) != SplashString.instance) {
                proc.error(this.modifiers.get(Modifier.accessor).range, "Accessor methods should only take 1 string parameter")
            }
        } else if (this.modifiers.has(Modifier.assigner)) {
            if (this.params.length != 2 || proc.resolveType(this.params[0].type) != SplashString.instance) {
                proc.error(this.modifiers.get(Modifier.assigner).range, "Assigner methods should take 2 parameters, the first one being string")
            }
        } else if (this.modifiers.has(Modifier.indexer)) {
            if (this.modifiers.has(Modifier.get)) {
                if (this.params.length != 1) {
                    proc.error(this.modifiers.get(Modifier.indexer).range, "Getter Indexer methods should only take 1 parameter")
                }
            } else if (this.modifiers.has(Modifier.set)) {
                if (this.params.length != 2) {
                    proc.error(this.modifiers.get(Modifier.indexer).range, "Setter Indexer methods should take 2 parameters")
                }
            }
        } else if (this.modifiers.has(Modifier.iterator)) {
            if (this.params.length > 0) { // todo: validate return type is array
                proc.error(this.modifiers.get(Modifier.iterator).range, "Iterator methods should not take any parameters")
            }
            if (!proc.resolveType(this.retType).canAssignTo(SplashArray.instance)) {
                proc.error(this.retType.range, "Iterator methods must return an array")
            }
        } else if (this.modifiers.has(Modifier.get)) {
            if (this.params.length > 0) {
                proc.error(this.modifiers.get(Modifier.get).range, "Getter methods should not take any parameters")
            }
        } else if (this.modifiers.has(Modifier.set)) {
            if (this.params.length != 1) {
                proc.error(this.modifiers.get(Modifier.set).range, "Setter methods should only take 1 parameter")
            }
        }
        
        let processedParams: Parameter[] = []

        for (let p of this.params) {
            processedParams.push(new Parameter(p.name.value,proc.resolveType(p.type),p.defValue !== undefined,p.vararg))
        }
        
        this.method = new Method(this.name.value, proc.resolveType(this.retType), processedParams, this.modifiers)
        let existing = type.declaredMethods.find(m=>m.name == this.name.value && Parameter.allParamsMatch(m.params,processedParams.map(p=>p.type)))
        if (!existing) {
            type.addMember(this.method)
            console.log('added method',this.name.value,'to',type.toString())
        }
    }

    process(proc: Processor): void {
        proc.validateType(this.retType)
        let uniqueNames: string[] = []

        let hasVararg = false
        proc.push()
        proc.inInstanceContext = !this.modifiers.has(Modifier.static)

        if (proc.currentClass) {
            for (let f of proc.currentClass.members) {
                if (f instanceof Field) {
                    proc.addVariable(Token.dummy(f.name),f.type)
                }
            }
        }

        for (let p of this.params) {
            p.process(proc)
            if (uniqueNames.includes(p.name.value)) {
                proc.error(p.name.range, "Duplicate parameter '" + p.name.value + "'")
            } else {
                uniqueNames.push(p.name.value)
                proc.addVariable(p.name, proc.resolveType(p.type))
            }
            if (hasVararg) {
                proc.error(p.name.range, "Vararg parameter must be the last parameter")
            } else if (p.vararg) {
                hasVararg = true
            }
        }

        if (this.body) {
            this.body.process(proc)
        }

        proc.pop()
        proc.inInstanceContext = false
    }

    generate(proc: Processor, cls: SplashClass): void {
        if (!this.method) throw 'Method node was not indexed!'
        for (let i = 0; i < this.params.length; i++) {
            this.method.params[i].defValue = this.params[i].defValue?.generate(proc)
        }
        this.method.body = this.body?.generate(proc)
    }
    
}

export class ClassDeclaration extends ASTNode {

    type: SplashType

    constructor(public name: Token, public typeParams: TypeParameterNode[], public body: ClassMember[], public modifiers: ModifierList) {
        super('class_decl')
        if (modifiers.has(Modifier.native)) {
            this.type = BuiltinTypes[this.name.value]
        } else {
            this.type = new SplashClass(name.value)
        }
    }

    index(proc: Processor) {
        if (!this.modifiers.has(Modifier.native)) {
            proc.types.push(this.type)

            let index = 0;
            for (let tp of this.typeParams) {
                tp.init(proc,index)
                if (tp.param) {
                    this.type.typeParams.push(tp.param)
                }
                index++;
            }
        }

        proc.currentClass = this.type

        for (let m of this.body) {
            m.index(proc, this.type)
        }

        proc.currentClass = undefined
    }

    process(proc: Processor): void {
        proc.currentClass = this.type
        proc.push()

        let uniqueTP: string[] = []
        for (let tp of this.typeParams) {
            tp.process(proc)
            if (uniqueTP.includes(tp.base.value)) {
                proc.error(tp.base.range,'Duplicate type parameter')
            } else {
                uniqueTP.push(tp.base.value)
            }
        }

        for (let m of this.body) {
            m.process(proc)
        }

        proc.pop()
        proc.currentClass = undefined
    }
    generate(proc: Processor): SplashType {
        proc.currentClass = this.type
        for (let m of this.body) {
            m.generate(proc,this.type)
        }
        
        proc.currentClass = undefined
        return this.type
    }
    
}

export class ConstructorParamNode extends ASTNode {
    constructor(public name: Token, public assignToField: boolean, public type?: TypeToken, public defValue?: Expression, public vararg?: boolean) {
        super('ctor_param')
    }

    process(proc: Processor) {
        if (!this.assignToField && this.type) {
            proc.validateType(this.type)
        } else if (!proc.currentClass?.getField(this.name.value)){
            proc.error(this.name.range, "No such field to auto-assign to!")
        }
        if (this.vararg && this.defValue) {
            proc.error(this.name.range, "A parameter cannot be both vararg and have a default value")
        }
        if (this.defValue) {
            let type = this.type ? proc.resolveType(this.type) : proc.currentClass?.getField(this.name.value)?.type
            if (type && !this.defValue.getResultType(proc).canAssignTo(type)) {
                proc.error(this.defValue.range, "This expression cannot be assigned to this parameter's type")
            }
        }
    }

    generate(proc: Processor): CtorParameter {
        let def: GeneratedExpression | undefined
        let type = this.type ? proc.resolveType(this.type) : proc.currentClass?.getField(this.name.value)?.type || SplashClass.object
        if (type instanceof SplashOptionalType) {
            def = new GenConstExpression(new Value(type,null))
        } else if (this.defValue) {
            def = this.defValue.generate(proc)
        }
        let p = new CtorParameter(this.name.value,type,this.assignToField,def !== undefined,this.vararg)
        p.defValue = def
        return p
    }
}

export class ConstructorNode extends ClassMember {
    
    ctor: Constructor | undefined
    constructor(public params: ConstructorParamNode[], public modifiers: ModifierList, public body?: CodeBlock) {
        super('constructor')
    }

    index(proc: Processor, owner: SplashType) {
        let processedParams: CtorParameter[] = []

        for (let p of this.params) {
            let type = p.type ? proc.resolveType(p.type) : proc.currentClass?.getField(p.name.value)?.type || SplashClass.object
            processedParams.push(new CtorParameter(p.name.value,type,p.assignToField,p.defValue !== undefined,p.vararg))
        }

        this.ctor = new Constructor(owner,processedParams,this.modifiers)
        owner.addMember(this.ctor)
    }

    process(proc: Processor): void {
        
        let uniqueNames: string[] = []
        let hasVararg = false
        proc.push()
        proc.inInstanceContext = true
        
        for (let p of this.params) {
            p.process(proc)
            let type = p.type ? proc.resolveType(p.type) : proc.currentClass?.getField(p.name.value)?.type || SplashClass.object
            if (uniqueNames.includes(p.name.value)) {
                proc.error(p.name.range, "Duplicate parameter '" + p.name.value + "'")
            } else {
                uniqueNames.push(p.name.value)
                proc.addVariable(p.name, type)
            }
            if (hasVararg) {
                proc.error(p.name.range, "Vararg parameter must be the last parameter")
            } else if (p.vararg) {
                hasVararg = true
            }
            
        }

        if (this.body) {
            this.body.process(proc)
        }

        proc.pop()
        proc.inInstanceContext = false
    }
    generate(proc: Processor, cls: SplashClass): void {
        if (!this.ctor) throw 'Constructor node was not indexed!'
        for (let i = 0; i < this.params.length; i++) {
            this.ctor.params[i].defValue = this.params[i].defValue?.generate(proc)
        }
        this.ctor.body = this.body?.generate(proc)
    }

}

export class FieldNode extends ClassMember {

    field: Field | undefined
    constructor(public name: Token, public type: TypeToken, public modifiers: ModifierList, public defValue?: Expression) {
        super('field')
    }

    index(proc: Processor, owner: SplashType) {
        this.field = new Field(this.name.value,this.modifiers,proc.resolveType(this.type))
        owner.addMember(this.field)
    }
    
    process(proc: Processor): void {
        proc.validateType(this.type)
        if (this.defValue) {
            let res = this.defValue.getResultType(proc)
            let myType = proc.resolveType(this.type)
            if (!res.canAssignTo(myType)) {
                proc.error(this.defValue.range,"Expression of type " + res + " cannot be applied to " + myType)
            }
        }
    }
    generate(proc: Processor, cls: SplashClass): void {
        if (!this.field) throw 'Field was not indexed!'
        this.field.init = this.defValue?.generate(proc)
    }
    
}

export class NullExpression extends Expression {

    constructor(token: Token) {
        super('null',token.range)
    }

    getResultType(proc: Processor): SplashType {
        return DummySplashType.null
    }
    generate(proc: Processor): GeneratedExpression {
        return new GenConstExpression(Value.null)
    }
    
}

export class ThisAccess extends Expression {

    constructor(token: Token) {
        super('this',token.range)
    }

    getResultType(proc: Processor): SplashType {
        if (!proc.currentClass) {
            proc.error(this.range,"Cannot use 'this' in this context")
            return DummySplashType.null
        } else if (!proc.inInstanceContext) {
            proc.error(this.range,"Cannot use 'this' in a static context")
            return DummySplashType.null
        }
        return proc.currentClass
    }
    generate(proc: Processor): GeneratedExpression {
        return new GenVarAccess('this')
    }
    
}

export class RepeatStatement extends Statement {
    
    constructor(label: Token, public expr: Expression, public run: Statement) {
        super('repeat',label.range)
    }

    process(proc: Processor): void {
        let type = this.expr.getResultType(proc)
        if (!type.canAssignTo(SplashInt.instance)) {
            proc.error(this.expr.range,"Int expression expected")
        }
        proc.push()
        this.run.process(proc)
        proc.pop()
    }
    generate(proc: Processor): GeneratedStatement {
        return new GeneratedRepeat(this.expr.generate(proc),this.run.generate(proc))
    }
}

export class ForStatement extends Statement {
    constructor(label: Token, public varname: Token, public iter: Expression, public then: Statement) {
        super('for',label.range)
    }

    process(proc: Processor): void {
        proc.push()
        let iterType = this.iter.getResultType(proc)
        let iterator = iterType.getIterator()
        if (!iterator) {
            proc.error(this.iter.range,"This value is not iterable!")
        } else {
            if (iterator.retType.canAssignTo(SplashArray.instance) && iterator.retType instanceof SplashParameterizedType) {
                let elemType = iterator.retType.params[0]
                proc.declareVariable(this.varname,elemType)
            }
        }
        this.then.process(proc)
        proc.pop()
    }
    generate(proc: Processor): GeneratedStatement {
        return new GeneratedFor(this.varname.value,this.iter.generate(proc),this.then.generate(proc))
    }
    
}