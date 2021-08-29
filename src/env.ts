import { SplashScript } from "./generator"
import * as fs from 'fs'
import * as paths from 'path'
import { Parser } from "./parser"
import { BaseTokenizer, TextRange } from "./tokenizer"
import { Processor } from "./processor"
import { RootNode } from "./ast"
import { NativeFunctions, NativeMethods } from "./native"


export function compileModule(path: string, sdk?: SplashModule) {
    console.log('compiling module',path)
    let module = new SplashModule(path)
    let asts: RootNode[] = []

    for (let f of fs.readdirSync(path)) {
        if (paths.extname(f) == '.splash') {
            let fp = paths.join(path,f)
            let ast = parseFile(fp)
            if (ast) {
                asts.push(ast)
            }
        }
    }
    
    let proc = new Processor()
    
    if (sdk) {
        proc.import(sdk)
    }
    
    for (let ast of asts) {
        ast.index(proc)
    }
    for (let ast of asts) {
        ast.indexChildren(proc)
    }
    
    NativeFunctions.init(proc)
    NativeMethods.init(proc)

    for (let ast of asts) {
        let script = processAndGenerate(proc,ast)
        if (script) {
            module.scripts.push(script)
        } else {
            module.valid = false
        }
    }
    return module
}

export function compileFile(file: string, sdk: SplashModule): SplashScript | undefined {
    let root = parseFile(file)
    if (root) {
        let proc = new Processor()

        if (sdk) {
            proc.import(sdk)
        }
        root.index(proc)
        let script = processAndGenerate(proc,root)
        return script
    }
    return
}

export function parseFile(file: string): RootNode | undefined {
    console.log('compiling script ' + file)
    console.time('compilation done')
    
    const input = fs.readFileSync(file).toString('utf-8')

    let tokenizer = new BaseTokenizer(input)
    let parser = new Parser(file, tokenizer)

    let root = parser.parseFile()
    console.timeEnd('compilation done')

    return parser.diagnostics.length > 0 ? undefined : root
}

export function processAndGenerate(proc: Processor, ast: RootNode) {
    console.log('processing ' + ast.file + '...')
    console.time('processing done')
    
    proc.process(ast)
    console.timeEnd('processing done')

    if (proc.diagnostics.length == 0) {
        console.log('generating...')
        console.time('generation done')
        let generated = ast.generate()
        console.timeEnd('generation done')
        return generated
    }
    return undefined
}

export class SplashModule {
    
    scripts: SplashScript[] = []
    valid: boolean = true

    constructor(public name: string) {

    }
}


export interface Diagnostic {
    file: string
    range: TextRange
    message: string
    type: DiagnosticType
}

export enum DiagnosticType {
    error = 1,
    warn = 2,
    info = 3,
    hint = 4
}

export enum CompletionType {
    variable = 6,
    field = 5,
    method = 2,
    keyword = 14,
    class = 7,
    typeParam = 25,
    function = 3
}

export interface PartialCompletion {
    value: string
    detail?: string
    desc?: string
    type?: CompletionType
}

export interface Completion extends PartialCompletion {
    range: TextRange
}

export interface TextLocation {
    file: string
    range: TextRange
}

export interface TokenInfo {
    range?: TextRange
    detail: string
    desc?: string
    declaration?: TextLocation
}