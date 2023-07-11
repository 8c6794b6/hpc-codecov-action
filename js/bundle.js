/*
  A script for bundling with esbuild
  ==================================

  This file contains an invokation of 'esbuild.build()' function to
  bundle the JavaScript codes generated from PureScript.

  As of spago 0.21.0, seems like there is no way to pass the 'banner'
  options to esbuild from spago's "bundle-app" command. So explicitly
  specifying from this file. The 'banner' option is used as a
  workaround for 'Dynamic require of "FOO" is not supported' error
  mentioned in:

    https://github.com/evanw/esbuild/issues/1921

  which is occuring from the compiled output of GitHub actions toolkit
  binding for PureScript. When the issue is resolved, this file could
  be removed and 'spago bundle-app' command could be used instead.
 */

import * as esbuild from 'esbuild';

await esbuild.build({
    entryPoints: ['js/entry.js'],
    bundle: true,
    minify: true,
    treeShaking: true,
    sourcemap: true,
    platform: 'node',
    format: 'esm',
    outfile: 'dist/main.mjs',
    logLevel: 'info',
    banner: {
	js: `import {createRequire as __crq} from 'module';` +
	    `const require=__crq(import.meta.url);`
    }
});
