import { SplashScript } from "./generator"
import * as fs from 'fs'
import * as paths from 'path'
import { Parser } from "./parser"
import { BaseTokenizer } from "./tokenizer"
import { Processor } from "./processor"
import { RootNode } from "./ast"
import { NativeFunctions, NativeMethods } from "./native"


export function compileModule(path: string, sdk?: SplashModule) {
    let module = new SplashModule(paths.normalize(path))
    let asts: RootNode[] = []

    let proc = new Processor()
    NativeFunctions.init(proc)
    NativeMethods.init(proc)

    if (sdk) {
        proc.import(sdk)
    }

    for (let f of fs.readdirSync(path)) {
        if (paths.extname(f) == '.splash') {
            let fp = paths.join(path,f)
            let ast = parseFile(fp)
            if (ast) {
                asts.push(ast)
            }
        }
    }

    for (let ast of asts) {
        ast.index(proc)
    }

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

    return parser.hasErrors ? undefined : root
}

export function processAndGenerate(proc: Processor, ast: RootNode) {
    console.log('processing ' + ast.file + '...')
    console.time('processing done')
    
    proc.process(ast)
    console.timeEnd('processing done')

    if (!proc.hasErrors) {
        console.log('generating...')
        console.time('generation done')
        let generated = ast.generate(proc)
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