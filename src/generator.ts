import { Expression, SwitchCase } from "./ast";
import { NativeFunctions } from "./native";
import { Parameter, Value } from "./oop";
import { AssignmentOperator, BinaryOperator, UnaryOperator } from "./operators";
import { SplashArray, SplashBoolean, SplashClass, SplashClassType, SplashComboType, SplashFloat, SplashFunctionType, SplashInt, SplashOptionalType, SplashString, SplashType } from "./types";
import { Break, Continue, Returned, Runtime, SplashRuntimeError } from "./runtime";
import { TokenType } from "./tokenizer";
import { TextLocation } from "./env";


export abstract class Generated {
    
}

export abstract class GeneratedStatement extends Generated {

    abstract run(runtime: Runtime): void

}


export class GeneratedBlock extends GeneratedStatement {

    constructor(private statements: GeneratedStatement[]) {
        super()
    }

    run(runtime: Runtime) {
        try {
            for (let s of this.statements) {
                //console.log("running statement " + s.constructor.name)
                s.run(runtime)
            }
        } catch (e) {
            if (e instanceof Returned || e instanceof Break || e instanceof Continue || e instanceof SplashRuntimeError) {
                throw e
            } else {
                console.log("Unexpected",e)
            }
        }
    }
    
}


export class SplashScript {

    functions: SplashFunction[] = []
    vars: GenVarDeclaration[] = []
    classes: SplashType[] = []
    main?: GeneratedBlock

    constructor(public name: string) {
        
    }

    run(runtime: Runtime) {
        runtime.include(this)
        if (this.main) {
            console.info("Executing script " + this.name)
            try {
                this.main.run(runtime)
            } catch (e) {
                if (e instanceof SplashRuntimeError) {
                    console.log('Runtime Error:',e.message)
                } else {
                    console.log('Unexpected:',e)
                }
            }
        }
    }

}

export class GenVarDeclaration extends GeneratedStatement {

    constructor(private name: string, private value?: GeneratedExpression) {
        super()
    }

    run(runtime: Runtime): void {
        runtime.declareVariable(this.name, this.value?.evaluate(runtime))
    }
    
}

export class SplashFunction {

    constructor(public name: string, public retType: SplashType, public params: Parameter[],  public decl?: TextLocation, public body?: GeneratedBlock, public docs?: string) {

    }

    toFunctionType() {
        return new SplashFunctionType(this.retType, this.params.map(p=>p.type))
    }

    invoke(runtime: Runtime, ...params: Value[]): Value {
        let r = runtime.copy()
        if (this.body) {
            for (let i = 0; i < params.length; i++) {
                let pv = params[i]
                let p = Parameter.getParamAt(i,this.params)
                if (p) {
                    r.setVariable(p.name,pv)
                }
            }
            try {
                this.body.run(r)
            } catch (e) {
                if (e instanceof Returned) {
                    return e.value
                } else {
                    throw e
                }
            }
            
            return Value.void
        } else {
            return NativeFunctions.invoke(r, this.name, params)
        }
    }

}


export abstract class GeneratedExpression extends Generated {

    abstract evaluate(runtime: Runtime): Value
}

export class GenStatementExpression extends GeneratedExpression {
    

    constructor(public statement: GeneratedStatement) {
        super()
    }

    evaluate(runtime: Runtime): Value {
        this.statement.run(runtime)
        return Value.void
    }
    
}

export class GenExpressionStatement extends GeneratedStatement {
    

    constructor(public expr: GeneratedExpression) {
        super()
    }

    run(runtime: Runtime): void {
        this.expr.evaluate(runtime)
    }
    
}



export class GenIfStatement extends GeneratedExpression {
    constructor(public expr: GeneratedExpression, public then: GeneratedExpression, public orElse?: GeneratedExpression) {
        super()
    }
    evaluate(runtime: Runtime): Value {
        let res = this.expr.evaluate(runtime)
        if (res.toBoolean(runtime)) {
            return this.then.evaluate(runtime)
        } else {
            return this.orElse?.evaluate(runtime) || Value.null
        }
    }
    
}

export abstract class GenAssignableExpression extends GeneratedExpression {

    evaluate(runtime: Runtime): Value {
        return this.evalSelf(runtime, this.evalParent(runtime))
    }

    abstract assign(runtime: Runtime, parent: Value | undefined, value: Value): void

    abstract evalParent(runtime: Runtime): Value | undefined

    abstract evalSelf(runtime: Runtime, parent?: Value): Value

}

export class GeneratedBinary extends GeneratedExpression {

    constructor(private left: GeneratedExpression, private op: BinaryOperator, private right: GeneratedExpression) {
        super()
    }

    evaluate(runtime: Runtime): Value {
        if (this.op == BinaryOperator.default) {
            let left = this.left.evaluate(runtime)
            if (left.isNull) {
                return this.right.evaluate(runtime)
            }
            return left
        }
        if (this.op == BinaryOperator.as) {
            this.right.evaluate(runtime)
            return this.left.evaluate(runtime)
        }
        if (this.op == BinaryOperator.is) {
            let val = this.left.evaluate(runtime)
            let type = this.right.evaluate(runtime)
            if (type.type instanceof SplashClassType) {
                return new Value(SplashBoolean.instance,val.type.canAssignTo(type.type.type))
            }
            return new Value(SplashBoolean.instance,false)
        }
        return this.left.evaluate(runtime).invokeBinOperator(runtime,this.op,this.right.evaluate(runtime))
    }
    

}

export class GenCallAccess extends GeneratedExpression {

    constructor(private expr: GeneratedExpression, private params: GeneratedExpression[]) {
        super()
    }

    evaluate(runtime: Runtime): Value {
        if (this.expr instanceof GenFieldAccess) {
            let fa = this.expr as GenFieldAccess
            return fa.expr.evaluate(runtime).invokeMethod(runtime,fa.field,...this.params.map(e=>e.evaluate(runtime)))
        } else if (this.expr instanceof GenVarAccess) {
            let va = this.expr as GenVarAccess
            return runtime.invokeFunction(va.name,...this.params.map(e=>e.evaluate(runtime)))
        }
        let val = this.expr.evaluate(runtime)
        return val.invoke(runtime, ...this.params.map(e=>e.evaluate(runtime)))
    }

}

export class GenCall extends GeneratedStatement {

    constructor(private call: GenCallAccess) {
        super()
    }

    run(runtime: Runtime): void {
        this.call.evaluate(runtime)
    }
    
}

export class GenIndexAccess extends GenAssignableExpression {

    constructor(public index: GeneratedExpression, public parent: GeneratedExpression) {
        super()
    }

    assign(runtime: Runtime, parent: Value | undefined, value: Value): void {
        if (parent) {
            parent.setIndex(runtime, this.index.evaluate(runtime), value)
        }
    }
    evalParent(runtime: Runtime): Value | undefined {
        return this.parent.evaluate(runtime)
    }
    evalSelf(runtime: Runtime, parent?: Value): Value {
        return parent?.getIndex(runtime, this.index.evaluate(runtime)) || Value.null
    }
    
}

export class GenFieldAccess extends GenAssignableExpression {
    
    constructor(public expr: GeneratedExpression, public field: string) {
        super()
    }

    assign(runtime: Runtime, parent: Value | undefined, value: Value): void {
        if (parent) {
            parent.set(runtime, this.field, value)
        }
    }
    evalParent(runtime: Runtime): Value | undefined {
        return this.expr.evaluate(runtime)
    }
    evalSelf(runtime: Runtime, parent?: Value): Value {
        return parent?.get(runtime, this.field) || Value.null
    }
    
}

export class GenVarAccess extends GenAssignableExpression {
    
    constructor(public name: string) {
        super()
    }

    assign(runtime: Runtime, parent: Value | undefined, value: Value): void {
        runtime.setVariable(this.name, value)
    }
    evalParent(runtime: Runtime): Value | undefined {
        return
    }
    evalSelf(runtime: Runtime, parent?: Value): Value {
        return runtime.getVariable(this.name) || Value.null
    }
}

export class GenAssignment extends GeneratedStatement {

    constructor(public variable: GenAssignableExpression, public op: AssignmentOperator, public expr: GeneratedExpression) {
        super()
    }

    run(runtime: Runtime): void {
        let parent = this.variable.evalParent(runtime)
        let val = this.expr.evaluate(runtime);
        let toAssign: Value
        
        if (this.op == AssignmentOperator.set) {
            toAssign = val
        } else {
            let self = this.variable.evalSelf(runtime)
            toAssign = self.invokeBinOperator(runtime, this.op.substring(0,this.op.length-1) as BinaryOperator, val)
        }
        if (!(toAssign.type instanceof SplashOptionalType)) {
            this.variable.assign(runtime, parent, toAssign)
        }
    }
    
}

export class GeneratedUnary extends GeneratedExpression {
    constructor(private expr: GeneratedExpression, private op: UnaryOperator) {
        super()
    }
    evaluate(runtime: Runtime): Value {
        return this.expr.evaluate(runtime).invokeUnaryOperator(runtime,this.op)
    }
    
}

export class GeneratedLiteral extends GeneratedExpression {
    static invalid = new GeneratedLiteral(TokenType.invalid, "")

    constructor(public type: TokenType, public value: string) {
        super()
    }
    evaluate(runtime: Runtime): Value {
        switch (this.type) {
            case TokenType.int:
                return new Value(SplashInt.instance, parseInt(this.value))
            case TokenType.string:
                return new Value(SplashString.instance, this.value)
            case TokenType.float:
                return new Value(SplashFloat.instance, parseFloat(this.value))
        }
        if (this.value == 'true' || this.value == 'false') {
            return new Value(SplashBoolean.instance, this.value == 'true')
        }
        return Value.null
    }
    
}

export class GenStringLiteral extends GeneratedExpression {
    
    constructor(public nodes: GeneratedExpression[]) {
        super()
    }

    evaluate(runtime: Runtime): Value {
        let str = "";
        for (let n of this.nodes) {
            let v = n.evaluate(runtime)
            if (v.type == SplashString.instance) {
                str += v.inner
            } else {
                str += v.toString(runtime)
            }
        }
        return new Value(SplashString.instance, str)
    }
}

export class GenArrayCreation extends GeneratedExpression {
    constructor(public values: GeneratedExpression[]) {
        super()
    }
    evaluate(runtime: Runtime): Value {
        let vals = this.values.map(v=>v.evaluate(runtime))
        let valueType: SplashType = SplashClass.object
        for (let v of vals) {
            if (valueType == SplashClass.object) {
                valueType = v.type
            } else if (valueType != v.type) {
                if (valueType instanceof SplashComboType) {
                    valueType = new SplashComboType([v.type,...valueType.types])
                } else {
                    valueType = new SplashComboType([valueType,v.type])
                }
            }
        }
        return new Value(SplashArray.of(valueType), vals)
    }
    
}

export class GeneratedReturn extends GeneratedStatement {
    constructor(public expr?: GeneratedExpression) {
        super()
    }
    run(runtime: Runtime): void {
        throw new Returned(this.expr?.evaluate(runtime) || Value.void)
    }
    
}

export class GenClassDecl extends GeneratedStatement {
    constructor(public type: SplashClass) {
        super()
    }
    run(runtime: Runtime): void {
        
    }
}

export class GenConstExpression extends GeneratedExpression {
    
    constructor(public value: Value) {
        super()
    }

    evaluate(runtime: Runtime): Value {
        return this.value
    }
    
}

export class GeneratedRepeat extends GeneratedStatement {
    constructor(public expr: GeneratedExpression, public then: GeneratedStatement) {
        super()
    }
    run(runtime: Runtime): void {
        let times = this.expr.evaluate(runtime)
        if (times.type.canAssignTo(SplashInt.instance)) {
            for (let i = 0; i < times.inner; i++) {
                try {
                    this.then.run(runtime)
                } catch (e) {
                    if (e instanceof Break) {
                        break
                    } else if (!(e instanceof Continue)) {
                        throw e
                    }
                }
            }
        }
    }
    
}

export class GeneratedFor extends GeneratedStatement {

    constructor(public varname: string, public iter: GeneratedExpression, public then: GeneratedStatement) {
        super()
    }

    run(runtime: Runtime): void {
        runtime.declareVariable(this.varname)
        let iterated = this.iter.evaluate(runtime)
        let arr = iterated.invokeIterator(runtime)
        if (arr.type.canAssignTo(SplashArray.instance)) {
            for (let val of arr.inner) {
                runtime.setVariable(this.varname,val)
                try {
                    this.then.run(runtime)
                } catch (e) {
                    if (e instanceof Break) {
                        break
                    } else if (!(e instanceof Continue)) {
                        throw e
                    }
                }
            }
        } else {
            console.log('value returned from iterator is not an array')
        }
    }
    
}

export class GeneratedWhile extends GeneratedStatement {
    constructor(public expr: GeneratedExpression, public then: GeneratedStatement) {
        super()
    }
    run(runtime: Runtime): void {
        let cond = this.expr.evaluate(runtime)
        while (cond.toBoolean(runtime)) {
            try {
                this.then.run(runtime)
            } catch (e) {
                if (e instanceof Break) {
                    break
                } else if (!(e instanceof Continue)) {
                    throw e
                }
            }
            cond = this.expr.evaluate(runtime)
        }
    }
}

export class GenBreakContinue extends GeneratedStatement {

    static break = new GenBreakContinue('break')
    static continue = new GenBreakContinue('continue')

    constructor(public type: 'break' | 'continue') {
        super()
    }
    run(runtime: Runtime): void {
        if (this.type == 'break') {
            throw new Break();
        } else if (this.type == 'continue') {
            throw new Continue();
        }
    }
    
}


export class GeneratedSwitch extends GeneratedExpression {
    constructor(public expr: GeneratedExpression, public cases: SwitchCase[], public defaultCase?: GeneratedExpression) {
        super()
    }
    evaluate(runtime: Runtime): Value {
        let val = this.expr.evaluate(runtime)

        for (let c of this.cases) {
            let matched = false
            for (let con of c.constraints) {
                if (con.match(runtime, val)) {
                    matched = true
                }
            }
            if (matched) {
                return c.expr.generate().evaluate(runtime)
            }
        }

        return this.defaultCase?.evaluate(runtime) || Value.null
    }
}