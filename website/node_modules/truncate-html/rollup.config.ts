import buble from 'rollup-plugin-buble'
import typescript from 'rollup-plugin-typescript2'

const pkg = require('./package.json')

function getConfig () {
  return {
    input: 'src/truncate.ts',
    output: [{
      name: 'truncate-html',
      banner: `/*!
 * trancate-html v${pkg.version}
 * Copyright© ${new Date().getFullYear()} Saiya ${pkg.homepage}
 */`,
      format: 'cjs',
      file: `dist/truncate.cjs.js`
    }, {
      name: 'truncate-html',
      banner: `/*!
 * trancate-html v${pkg.version}
 * Copyright© ${new Date().getFullYear()} Saiya ${pkg.homepage}
 */`,
      format: 'es',
      file: `dist/truncate.es.js`
    }],
    plugins: [
      typescript({
        tsconfigOverride: {
          compilerOptions: {
            declaration: true,
            module: 'esnext'
          }
        },
        typescript: require('typescript')
      }),
      buble()
    ],
    external: ['cheerio']
  }
}

export default getConfig()
