var path = require('path')
var webpack = require('webpack')

var CopyWebpackPlugin = require('copy-webpack-plugin')
var HtmlWebpackPlugin = require('html-webpack-plugin')
var MiniCssExtractPlugin = require('mini-css-extract-plugin')
var OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin')

module.exports = {
    entry: ['./web/scss/main.scss', './web/script.js'],

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
            title: 'Data Stewardship Wizard',
            template: 'web/index.ejs',
            filename: '../index.html'
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
    ],

    devServer: {
        inline: true,
        stats: { colors: true },
        historyApiFallback: { disableDotRule: true }
    }
}
