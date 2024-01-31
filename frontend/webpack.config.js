const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  // devServer: {
  //   headers: {
  //     "Access-Control-Allow-Origin": "*",
  //     "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
  //     "Access-Control-Allow-Headers": "X-Requested-With, content-type, Authorization"
  //   },
  //   proxy: {
  //     '/api': {
  //        target: {
  //           host: "localhost",
  //           protocol: 'http:',
  //           port: 3000
  //        },
  //        pathRewrite: {
  //           '^/api': ''
  //        }
  //     }
  //  }
  // },
  devServer: {
    allowedHosts: ['godel.moncrief.dev','all'],
    host: '0.0.0.0',
    port: 8081,
  },
    mode: "development",
    entry: path.join(__dirname, "src", "main.js"),
    output: {
        path: path.resolve(__dirname, 'dist'),
    },
    module: {
        rules: [
          {
            test: /\.(sa|sc|c)ss$/,
            use: [
              {
                loader: "style-loader",
              },
              {
                loader: "css-loader",
                options: {
                  sourceMap: true,
                },
              },
              {
                loader: "sass-loader",
                options: {
                  sourceMap: true,
                },
              },
            ],
          },
          {
            test: /\.(?:js|mjs|cjs)$/,
            exclude: /node_modules/,
            use: {
              loader: 'babel-loader',
              options: {
                presets: [
                  ['@babel/preset-env', { targets: "defaults" },],
                  ['@babel/preset-react', { targets: "defaults", runtime: 'automatic' }]
                ]
              }
            }
          }
        ],
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: path.join(__dirname, "src", "index.html"),
        }),
    ],
};