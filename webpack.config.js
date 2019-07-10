var path = require('path')
var webpack = require('webpack')

var CopyWebpackPlugin = require('copy-webpack-plugin')
var HtmlWebpackPlugin = require('html-webpack-plugin')
var MiniCssExtractPlugin = require('mini-css-extract-plugin')
var OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin')

module.exports = {
    entry: [
        './web/scss/main.scss',
        './web/style.css',
        './web/script.js',
        './web/specs.js'
    ],

    output: {
        path: path.resolve(__dirname + '/dist/static'),
        filename: '[name].[chunkhash].js'
    },

    module: {
        rules: [
            {
                test: /\.(scss|css)$/,
                use: [{ loader: MiniCssExtractPlugin.loader },
                    'css-loader',
                    'sass-loader'
                ]
            },
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]'
            }
        ]
    },

    plugins: [
        new HtmlWebpackPlugin({
            title: 'DSW Storage Costs Evaluator',
            template: "!!ejs-compiled-loader!./web/index.ejs",
            filename: '../index.html',
            minify: {
                removeComments: true,
                collapseWhitespace: true
            }
        }),
        new MiniCssExtractPlugin({
            filename: '[name].[chunkhash].css',
            allChunks: true
        }),
        new OptimizeCssAssetsPlugin({
            cssProcessorPluginOptions: {
                preset: ['default', { discardComments: { removeAll: true } }]
            }
        }),
        new webpack.ProvidePlugin({
            $: "jquery",
            jQuery: "jquery"
        }),
        new CopyWebpackPlugin([
            { from: 'web/img', to: 'img' },
            { from: 'web/favicon.ico', to: 'favicon.ico' }
        ])
    ]
}
