import { Value } from "./oop";
import { SplashBoolean, SplashType } from "./types";


export enum BinaryOperator {
    add = '+',
    sub = '-',
    mul = '*',
    div = '/',
    mod = '%',
    pow = '**',
    int_div = '//',
    lt = '<',
    gt = '>',
    le = '<=',
    ge = '>=',
    equals = '==',
    ne = '!=',
    is = 'is',
    in = 'in',
    as = 'as',
    range = '..',
    and = '&&',
    or = '||',
    default = '~'
}

export function isBidirectional(op: BinaryOperator) {
    switch (op) {
        case BinaryOperator.add:
        case BinaryOperator.mul:
        case BinaryOperator.equals:
        case BinaryOperator.ne:
            return true
    }
    return false
}

export function getOpMethodName(op: BinaryOperator) {
    switch (op) {
        case BinaryOperator.lt:
        case BinaryOperator.gt:
        case BinaryOperator.ge:
        case BinaryOperator.le:
        case BinaryOperator.equals:
        case BinaryOperator.ne:
            return 'compare'
    }
    return Object.entries(BinaryOperator).find(e=>e[1] == op)?.[0] || ''
}

export function transformOperatorResult(res: Value, methodName: string, op: BinaryOperator): Value {
    if (methodName == 'compare') {
        return new Value(SplashBoolean.instance,getComparisonResult(res, op))
    }
    return res
}

export function getActualOpReturnType(op: BinaryOperator, def: SplashType) {
    if (getOpMethodName(op) == 'compare') return SplashBoolean.instance
    return def
}

function getComparisonResult(res: Value, op: BinaryOperator) {
    switch (op) {
        case BinaryOperator.lt:
            return res.inner < 0
        case BinaryOperator.gt:
            return res.inner > 0
        case BinaryOperator.ge:
            return res.inner >= 0
        case BinaryOperator.le:
            return res.inner <= 0
        case BinaryOperator.equals:
            return res.inner == 0
        case BinaryOperator.ne:
            return res.inner != 0
    }
}

export enum UnaryOperator {
    positive = '+',
    negative = '-',
    not = '!',
    range = '..'
}

export enum AssignmentOperator {
    set = '=',
    add = '+=',
    sub = '-=',
    mul = '*=',
    div = '/=',
    mod = '%=',
    int_div = '//=',
    pow = '**='
}

export enum Modifier {
    private,
    protected,
    abstract,
    native,
    final,
    static,
    readonly,
    operator,
    iterator,
    get,
    set,
    indexer,
    accessor,
    assigner,
    invoker
}
