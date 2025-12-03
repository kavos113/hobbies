const path = require('path');

require('esbuild').build({
    entryPoints: [path.resolve(__dirname, 'build/resources/main/sample.js')],
    bundle: true,
    outfile: path.resolve(__dirname, 'build/resources/main/bundle.js'),
    platform: 'node',
    format: 'cjs',
}).catch(() => process.exit(1));