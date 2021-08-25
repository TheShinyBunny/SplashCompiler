
import path from 'path'
import { compileFile, compileModule, Diagnostic, DiagnosticType } from './env'
import { Parser } from './parser'
import { Processor } from './processor'
import { Runtime } from './runtime'
import { BaseTokenizer, TextRange } from './tokenizer'

export const sdk = compileModule(path.resolve(__dirname,'../sdk'))

/* const file = './test.splash'

if (sdk.valid) {
    let compiled = compileFile(file,sdk)

    if (compiled) {
        console.log(compiled)
        console.log('executing...')
        console.time('execution done')
        let rt = new Runtime()
        rt.includeModule(sdk)
        compiled.run(rt)
        console.timeEnd('execution done')
    }
} */


export {
    compileFile, 
    compileModule, 
    Diagnostic, 
    DiagnosticType,
    Parser,
    BaseTokenizer,
    Processor,
    TextRange,
    Runtime
}