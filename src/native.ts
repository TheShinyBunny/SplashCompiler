import { Method, Parameter, Value } from "./oop";
import { Modifier } from "./operators";
import { Runtime, SplashRuntimeError } from "./runtime";
import { BuiltinTypes, SplashArray, SplashBoolean, SplashClass, SplashClassType, SplashInt, SplashString, SplashType } from "./types";
import { ModifierList } from "./ast";
import { Processor } from "./processor";

interface UnbakedNativeFunction {
    name: string
    retType: string
    params?: string[]
    run: (r: Runtime, ...params: Value[])=>Value
}

export interface NativeFunction {
    name: string
    retType: SplashType
    params: Parameter[]
    run: (r: Runtime, ...params: Value[])=>Value
}

function NativeFunction(retType: string, params?: string[]) {
    return function(target: any, key: string, desc: PropertyDescriptor) {
        unbakedFunctions.push({
            name: key,
            retType,
            params,
            run: desc.value
        })
    }
}
const unbakedFunctions: UnbakedNativeFunction[] = []
export const nativeFunctions: NativeFunction[] = []


export class NativeFunctions {
    
    static invoke(r: Runtime, name: string, params: Value[]) {
        for (let e of nativeFunctions) {
            if (e.name == name && Parameter.allParamsMatch(e.params,params.map(p=>p.type))) {
                return e.run(r,...params)
            }
        }
        console.log('no native function found named ' + name)
        return Value.null
    }

    static init(proc: Processor) {
        for (let u of unbakedFunctions) {
            nativeFunctions.push({
                name: u.name,
                retType: proc.resolveTypeFromString(u.retType) || SplashClass.object,
                params: u.params?.map(p=>Parameter.readFromString(p,proc)) || [],
                run: u.run
            })
        }
        
    }

    @NativeFunction('void',['object msg'])
    print(r: Runtime, msg: Value) {
        console.log(">> " + msg.toString(r))
        return Value.void
    }

    @NativeFunction('string',['string? query'])
    readLine(r: Runtime, msg: Value) {
        let res;
        if (msg.isNull) {
            res = r.prompt({})
        } else {
            res = r.prompt(msg.inner as string)
        }
        return new Value(SplashString.instance, res)
    }

}

interface UnbakedNativeMethod {
    name: string
    type: string
    retType: string
    params?: string[]
    modifiers?: Modifier[]
    run: (r: Runtime, thisArg?: Value, ...params: Value[])=>Value
}

export interface NativeMethod {
    name: string
    type: SplashType
    retType: SplashType
    params: Parameter[]
    run: (r: Runtime, thisArg?: Value, ...params: Value[])=>Value
}

function NativeMethod(retType: string, params?: string[], modifiers?: Modifier[], altname?: string) {
    return function(target: any, key: string, desc: PropertyDescriptor) {
        let type = key.substring(0,key.indexOf('_'))
        let name = altname || key.substring(key.indexOf('_') + 1)
        unbakedMethods.push({
            name,
            type,
            retType,
            params,
            modifiers,
            run: desc.value
        })
    }
}

const unbakedMethods: UnbakedNativeMethod[] = []
const nativeMethods: NativeMethod[] = []

export class NativeMethods {

    static invoke(r: Runtime, type: SplashType, name: string, params: Value[], thisArg?: Value) {
        if (type instanceof SplashClassType) {
            type = type.type
        }
        let e = NativeMethods.findMethod(type,name,params.map(p=>p.type))
        if (e) {
            return e.run(r,thisArg,...params)
        }
        console.log('no native method found in ' + type + ' named ' + name)
        return Value.null
    }

    static findMethod(type: SplashType, name: string, paramTypes: SplashType[]) {
        for (let e of nativeMethods) {
            if (type.canAssignTo(e.type) && e.name == name && Parameter.allParamsMatch(e.params.map(p=>p.resolve(type)),paramTypes)) {
                return e
            }
        }
    }

    static init(proc: Processor) {
        for (let u of unbakedMethods) {
            let inType = proc.resolveTypeFromString(u.type) || SplashClass.object
            proc.currentClass = inType
            let ret = proc.resolveTypeFromString(u.retType) || SplashClass.object
            let par = u.params?.map(p=>Parameter.readFromString(p,proc,inType)) || []
            proc.currentClass = undefined
            //inType.addMember(new Method(u.name,ret,par,new ModifierList(u.modifiers)))
            console.log('added native method ' + u.name + ' to',inType.toString())
            nativeMethods.push({
                name: u.name,
                type: inType,
                retType: ret,
                params: par,
                run: u.run
            })
        }
    }

    // STRING
    @NativeMethod('string')
    string_toLowerCase(r: Runtime, val: Value) {
        return new Value(SplashString.instance,val.inner.toLowerCase())
    }

    @NativeMethod('string')
    string_toUpperCase(r: Runtime, val: Value) {
        return new Value(SplashString.instance,val.inner.toUpperCase())
    }

    @NativeMethod('string[]')
    string_chars(r: Runtime, val: Value) {
        return new Value(SplashArray.of(SplashString.instance),val.inner.split(""))
    }

    @NativeMethod('string',['int chars'],[Modifier.operator])
    string_sub(r: Runtime, val: Value, chars: Value) {
        return new Value(SplashString.instance,val.inner.substring(0,val.inner.length - chars.inner))
    }

    @NativeMethod('int',['string other'],[Modifier.operator])
    string_compare(r: Runtime, str: Value, other: Value) {
        return new Value(SplashInt.instance,str.inner.localeCompare(other.inner))
    }

    // INT
    @NativeMethod('int',['int other'],[Modifier.operator])
    int_add(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.int,val.inner + other.inner)
    }
    @NativeMethod('float',['float other'],[Modifier.operator],'add')
    int_fadd(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner + other.inner)
    }

    @NativeMethod('int',['int other'],[Modifier.operator])
    int_sub(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.int,val.inner - other.inner)
    }

    @NativeMethod('float',['float other'],[Modifier.operator],'sub')
    int_fsub(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner - other.inner)
    }

    @NativeMethod('int',['int other'],[Modifier.operator])
    int_mul(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.int,val.inner * other.inner)
    }

    @NativeMethod('float',['float other'],[Modifier.operator],'mul')
    int_fmul(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner * other.inner)
    }

    @NativeMethod('float',['int | float other'],[Modifier.operator])
    int_div(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner / other.inner)
    }

    @NativeMethod('int',['int | float other'],[Modifier.operator])
    int_int_div(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.int,Math.floor(val.inner / other.inner))
    }

    @NativeMethod('int',['int | float other'],[Modifier.operator])
    int_mod(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.int,val.inner % other.inner)
    }

    @NativeMethod('int',['int other'],[Modifier.operator])
    int_pow(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.int,Math.pow(val.inner, other.inner))
    }

    @NativeMethod('float',['float other'],[Modifier.operator],'pow')
    int_fpow(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,Math.pow(val.inner,other.inner))
    }

    @NativeMethod('int',[],[Modifier.operator])
    int_negative(r: Runtime, val: Value) {
        return new Value(SplashInt.instance, -val.inner)
    }

    @NativeMethod('int',['string value'],[Modifier.static])
    int_parse(r: Runtime, _: Value, str: Value) {
        let res = parseInt(str.inner)
        if (isNaN(res)) {
            throw new SplashRuntimeError("Invalid integer '" + str.inner + "'");
        }
        return new Value(SplashInt.instance, res)
    }

    // FLOAT
    @NativeMethod('float',['int | float other'],[Modifier.operator])
    float_add(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner + other.inner)
    }

    @NativeMethod('float',['int | float other'],[Modifier.operator])
    float_sub(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner - other.inner)
    }
    
    @NativeMethod('float',['int | float other'],[Modifier.operator])
    float_mul(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner * other.inner)
    }

    @NativeMethod('float',['int | float other'],[Modifier.operator])
    float_div(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner / other.inner)
    }

    @NativeMethod('int',['int | float other'],[Modifier.operator])
    float_int_div(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.int,Math.floor(val.inner / other.inner))
    }

    @NativeMethod('float',['int | float other'],[Modifier.operator])
    float_mod(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,val.inner % other.inner)
    }

    @NativeMethod('float',['int | float other'],[Modifier.operator])
    float_pow(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.float,Math.pow(val.inner, other.inner))
    }

    @NativeMethod('float',[],[Modifier.operator])
    float_negative(r: Runtime, val: Value) {
        return new Value(SplashInt.instance, -val.inner)
    }

    @NativeMethod('float',['string value'],[Modifier.static])
    float_parse(r: Runtime, _: Value, str: Value) {
        return new Value(SplashInt.instance, parseFloat(str.inner))
    }

    // BOOLEAN
    @NativeMethod('boolean',[],[Modifier.operator])
    boolean_not(r: Runtime, val: Value) {
        return new Value(BuiltinTypes.boolean,!val.inner)
    }

    @NativeMethod('boolean',['boolean other'],[Modifier.operator])
    boolean_or(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.boolean, val.inner || other.inner)
    }

    @NativeMethod('boolean',['boolean other'],[Modifier.operator])
    boolean_and(r: Runtime, val: Value, other: Value) {
        return new Value(BuiltinTypes.boolean, val.inner && other.inner)
    }

    // ARRAY
    @NativeMethod('T',['int index'],[Modifier.get,Modifier.indexer])
    array_indexer(r: Runtime, arr: Value, index: Value) {
        let i: number
        if (index.inner < 0) {
            i = arr.inner.length + index.inner
        } else {
            i = index.inner
        }
        if (i >= arr.inner.length || i < 0) {
            throw new SplashRuntimeError(`Array index out of range. Index: ${i}, Length: ${arr.inner.length}`)
        }
        return arr.inner[i]
    }

    @NativeMethod('void',['T value'])
    array_add(r: Runtime, arr: Value, val: Value) {
        arr.inner.push(val)
        return Value.void
    }

    @NativeMethod('void',['int index','T value'],[],'add')
    array_addat(r: Runtime, arr: Value, index: Value, val: Value) {
        if (index.inner > arr.inner.length) {
            throw new SplashRuntimeError(`Array index out of range for adding a value. Index: ${index.inner}, Length: ${arr.inner.length}`)
        }
        (arr.inner as Value[]).splice(index.inner,0,val)
        return Value.void
    }

    @NativeMethod('string')
    array_toString(r: Runtime, arr: Value) {
        return new Value(SplashString.instance,'[' + (arr.inner as Value[]).map(v=>v.toString(r)).join(', ') + ']')
    }

    @NativeMethod('T',['int index'])
    array_remove(r: Runtime, arr: Value, index: Value) {
        if (arr.inner.length <= index.inner) {
            throw new SplashRuntimeError('array.remove() is out of bounds. Length: ' + arr.inner.length + ', Index: ' + index.inner)
        }
        return (arr.inner as []).splice(index.inner,1)
    }

    @NativeMethod('int',[],[Modifier.get])
    array_length(r: Runtime, arr: Value) {
        return new Value(SplashInt.instance, arr.inner.length)
    }

    @NativeMethod('void')
    array_clear(r: Runtime, arr: Value) {
        arr.inner = []
    }

    // OBJECT
    @NativeMethod('this',['this def'],[Modifier.operator])
    object_default(r: Runtime, val: Value, other: Value) {
        return val.isNull ? other : val
    }

    @NativeMethod('string')
    object_toString(r: Runtime, val: Value) {
        return new Value(SplashString.instance,val.isPrimitive ? val.inner.toString() : val.type.name + ':' + val.uid)
    }

    @NativeMethod('int',['this other'])
    object_compare(r: Runtime, val: Value, other: Value) {
        return new Value(SplashInt.instance,val.uid - other.uid)
    }

}