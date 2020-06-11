// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template
const CopyPlugin = require('copy-webpack-plugin');

var path = require("path");

var production = process.argv.indexOf("-p") >= 0;
var debug = process.argv.indexOf("-g") >= 0;
var outPath = debug ? "./bin/Fable/Debug" : "./bin/Fable/Release";


module.exports = {
    mode: production ? "production" : "development",
    optimization:{
        minimize: !debug
    },
    entry: {
        bundle: path.join(__dirname, "./src/Demo/Demo.fsproj"),
    },
    output: {
        path: path.join(__dirname, outPath),
        filename: "[name].js",
    },
    devServer: {
        contentBase: path.join(__dirname, outPath),
        port: 8080,
        host: '0.0.0.0',
        headers: {
          'Access-Control-Allow-Origin': '*',
          'Access-Control-Allow-Headers': '*',
        },
        clientLogLevel: 'error'

    },
    devtool: (production && !debug) ? false : "eval-source-map",
    module: {
        rules: [
			{
				test: /\.fs(x|proj)?$/,
				use: {
					loader: "fable-loader",
                    options: {
                        define: (debug ? [] : [])
                    }
				}
			}
        ]
    },
	plugins: [
	  new CopyPlugin(
      {
          patterns: [

            { from: path.join(__dirname, "./src/Demo/index.html"), to: path.join(path.join(__dirname, outPath), "./index.html") },
            { from: path.join(__dirname, "./src/Demo/index.css"), to: path.join(path.join(__dirname, outPath), "./index.css") }
          ]
      })
    ]
}