
import { compileFile, compileModule } from './env'
import { Runtime } from './runtime'

const sdk = compileModule('./sdk')

console.log(sdk)
const file = './test.splash'

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
}
