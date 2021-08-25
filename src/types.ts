

import { ExpressionList } from "./ast"
import { TextLocation } from "./env"
import { Constructor, Field, Member, Method, Parameter, Value } from "./oop"
import { BinaryOperator, getOpMethodName, Modifier, UnaryOperator } from "./operators"
import { Processor } from "./processor"

export abstract class SplashType {

    protected _members: Member[] = []
    staticFields: {[name: string]: Value} = {}
    declaredMethods: Method[] = []
    typeParams: TypeParameter[] = []
    declaration?: TextLocation

    constructor(public name: string) {

    }
    
    get methods(): Method[] {
        return this.members.filter(m=>m instanceof Method).map(m=>m as Method)
    }

    get defaultValue(): Value {
        return Value.null
    }

    get members(): Member[] {
        let m = [...this._members]
        if (!(this instanceof SplashClass) || this != SplashClass.object) {
            m.push(...this.super.members)
        }
        return m
    }

    get super(): SplashType {
        return SplashClass.object
    }

    addMember(m: Member) {
        if (!this._members) {
            this._members = []
        }
        this._members.push(m)
        if (m instanceof Method) {
            this.declaredMethods.push(m)
        }
    }

    getBinaryOperation(op: BinaryOperator, right: SplashType): Method | undefined {
        let name = getOpMethodName(op)
        let methods = this.getMethods(name)
        for (let m of methods) {
            if (m.modifiers.has(Modifier.operator) && m.params[0] && right.canAssignTo(m.params[0].type)) {
                return m
            }
        }
    }

    getUnaryOperation(op: UnaryOperator): Method | undefined {
        let name = Object.entries(UnaryOperator).find(e=>e[1] == op)?.[0] || ''
        let methods = this.getMethods(name)
        for (let m of methods) {
            if (m.modifiers.has(Modifier.operator)) {
                return m
            }
        }
    }

    getMethods(name: string) {
        return this.methods.filter(m=>m.name == name)
    }

    getMembers(name: string) {
        return this.members.filter(m=>m.name == name)
    }

    getMemberTypes(proc: Processor, name: string) {
        return this.getMembers(name).map(m=>m.type.resolve(this))
    }

    getField(name: string) {
        return this.members.find(m=>m instanceof Field && m.name == name)
    }

    getInvoker(proc: Processor, params: ExpressionList): Method | undefined {
        for (let m of this.methods) {
            if (m.modifiers.has(Modifier.invoker) && params.canApplyTo(proc,m.params,false)) {
                return m
            }
        }
    }

    getIndexGetter(index: SplashType): Method | undefined {
        for (let m of this.methods) {
            if (m.modifiers.has(Modifier.get) && m.modifiers.has(Modifier.indexer) && m.params.length == 1 && index.canAssignTo(m.params[0].type)) {
                return m
            }
        }
    }

    getIndexSetter(index: SplashType, value?: SplashType): Method | undefined {
        for (let m of this.methods) {
            if (m.modifiers.has(Modifier.set) && m.modifiers.has(Modifier.indexer) && m.params.length == 2 && index.canAssignTo(m.params[0].type) && (!value || value.canAssignTo(m.params[1].type))) {
                return m
            }
        }
    }

    getIterator() {
        for (let m of this.methods) {
            if (m.modifiers.has(Modifier.iterator) && m.params.length == 0) {
                return m
            }
        }
    }

    getValidMethod(name: string, ...params: SplashType[]) {
        return this.getMethods(name)
            .filter(m=>Parameter.allParamsMatch(m.params.map(p=>p.resolve(this)),params))[0]
    }

    canAssignTo(type: SplashType): boolean {
        if (this == type) return true
        if (type == SplashClass.object) return true
        
        if (type instanceof SelfSplashType) {
            return this.canAssignTo(type.base)
        }
        if (type instanceof SplashOptionalType) {
            return this == type.inner
        }
        if (type instanceof SplashComboType) {
            return type.types.some(t=>this.canAssignTo(t))
        }
        /* if (type instanceof SplashParameterizedType) {
            return this.canAssignTo(type.base)
        } */
        if (this instanceof SplashParameterizedType) {
            return this.base.canAssignTo(type)
        }
        return false
    }

    resolve(ownerType: SplashType): SplashType {
        return this
    }

    static combine(types: SplashType[]) {
        if (types.length == 1) {
            return types[0]
        }
        let flat: SplashType[] = []
        for (let t of types) {
            if (t instanceof SplashComboType) {
                flat.push(...t.types)
            } else {
                flat.push(t)
            }
        }
        return new SplashComboType(flat)
    }

    toString() {
        return this.name
    }
}

export class TypeParameter extends SplashType {

    constructor(name: string, public index: number, public extend?: SplashType) {
        super(name)
    }

    resolve(ownerType: SplashType): SplashType {
        if (ownerType instanceof SplashParameterizedType) {
            return ownerType.params[this.index]
        }
        return this
    }
}

export class SplashFunctionType extends SplashType {

    constructor(public retType: SplashType, public params: SplashType[]) {
        super('function')
    }

    get members(): Member[] {
        return []
    }

    toString() {
        return 'function(' + this.params.join(',') + '): ' + this.retType.toString()
    }

    resolve(ownerType: SplashType): SplashType {
        return new SplashFunctionType(this.retType.resolve(ownerType),this.params.map(p=>p.resolve(ownerType)))
    }
    
}

export abstract class SplashPrimitive extends SplashType {
    
    abstract get defaultValue(): Value

}

export class SplashInt extends SplashPrimitive {
    
    static instance = new SplashInt('int')
    
    get defaultValue(): Value {
        return new Value(this, 0)
    }
    
}


export class SplashFloat extends SplashPrimitive {
    
    static instance = new SplashFloat('float')
    
    get defaultValue(): Value {
        return new Value(this, 0.0)
    }
    
}


export class SplashArray extends SplashPrimitive {

    static instance = new SplashArray()

    constructor() {
        super('array')
        this.typeParams = [new TypeParameter('T',0)]
    }

    get defaultValue(): Value {
        return new Value(this, [])
    }
    
    static of(type: SplashType) {
        return new SplashParameterizedType(SplashArray.instance,[type])
    }
}

export class SplashString extends SplashPrimitive {
    
    static instance = new SplashString('string')

    get defaultValue(): Value {
        return new Value(this, "")
    }
}

export class DummySplashType extends SplashType {
    static void = new DummySplashType('void')
    static null = new DummySplashType('null')

    constructor(name: string) {
        super(name)
    }
    get members(): Member[] {
        return []
    }
    
}

export class SplashClass extends SplashType {
    static object = new SplashClass('object')

    constructor(name: string) {
        super(name)
    }

    get constructors(): Constructor[] {
        return this.members.filter(m=>m instanceof Constructor).map(m=>m as Constructor)
    }

    getValidCtor(params: Value[]) {
        return this.constructors.find(c=>Parameter.allParamsMatch(c.params,params.map(p=>p.type)))
    }
}

export class SplashParameterizedType extends SplashType {
    

    constructor(public base: SplashType, public params: SplashType[]) {
        super(base.name)
    }

    get members(): Member[] {
        return this.base.members
    }

    canAssignTo(type: SplashType) {
        if (type instanceof SplashParameterizedType) {
            console.log(this.base,type)
            let base = this.base.canAssignTo(type)
            let sl = this.params.length == type.params.length 
            let pm = false
            if (sl) {
                pm = this.params.every((t,i)=>t == type.params[i])
            }
            return base && sl && pm
        }
        return this.base.canAssignTo(type)
    }

    toString() {
        return this.base.toString() + '<' + this.params.join(',') + '>'
    }

}

export class SplashComboType extends SplashType {

    constructor(public types: SplashType[]) {
        super('union')
    }

    get members(): Member[] {
        return this.types.map(t=>t.members).reduce((prev,curr)=>prev.concat(curr),[])
    }

    canAssignTo(type: SplashType) {
        return this.types.find(t=>t.canAssignTo(type)) !== undefined
    }

}

export class SplashOptionalType extends SplashType {

    constructor(public inner: SplashType) {
        super('optional')
    }

    get members(): Member[] {
        return this.inner.members
    }

    toString() {
        return this.inner.toString() + '?'
    }

    canAssignTo(type: SplashType) {
        if (type == SplashClass.object) return true
        return type instanceof SplashOptionalType && this.inner.canAssignTo(type.inner)
    }

    static of(type: SplashType) {
        return new SplashOptionalType(type)
    }
    
}

export class SelfSplashType extends SplashType {

    constructor(public base: SplashType) {
        super('this')
    }
    
    toString() {
        return 'this'
    }

    canAssignTo(type: SplashType) {
        return true
    }
    
    resolve(ownerType: SplashType) {
        return ownerType
    }
}

export class SplashClassType extends SplashClass {

    constructor(public type: SplashType) {
        super(type.toString())
        this.typeParams = [new TypeParameter('T',0)]
    }

    static of(type: SplashType) {
        return new SplashClassType(type)
    }

    get members() {
        return this.type.members.filter(m=>m.isStatic)
    }
}

export class SplashBoolean extends SplashPrimitive {
    static instance = new SplashBoolean('boolean')
    get defaultValue(): Value {
        return new Value(this,false)
    }
    
}


export const BuiltinTypes: {[name: string]: SplashType} = {
    string: SplashString.instance,
    int: SplashInt.instance,
    array: SplashArray.instance,
    boolean: SplashBoolean.instance,
    object: SplashClass.object,
    float: SplashFloat.instance,
    null: DummySplashType.null,
    void: DummySplashType.void
}