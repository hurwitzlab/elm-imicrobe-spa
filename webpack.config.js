var path = require('path'),
    webpack = require('webpack');

module.exports = {
    entry: {
        app: [ './src/index.js' ]
    },
    output: {
        filename: '[name].js',
        path: path.resolve(__dirname, 'dist')
    },
    module: {
      noParse: /\.elm$/,
      rules: [
          {
            test: /\.(css|scss)$/,
            use: [
              'style-loader',
              'css-loader',
            ]
          },
          {
            test:    /\.html$/,
            exclude: /node_modules/,
            loader:  'file-loader?name=[name].[ext]',
          },
          {
            test:    /\.elm$/,
            exclude: [/elm-stuff/, /node_modules/],
            loader:  'elm-webpack-loader?verbose=true&warn=true',
          },
          {
            test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
            loader: 'url-loader?limit=10000&mimetype=application/font-woff',
          },
          {
            test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
            loader: 'file-loader',
          },
          {
            test: /\.(jpe?g|png|gif|ico)$/,
            loader: 'file-loader?name=./img/[name].[ext]',
          },
          {
            test: /vendor\/.+\.(jsx|js)$/,
            loader: 'imports-loader?jQuery=jquery,$=jquery,this=>window'
          },
          {
            test: /bootstrap\/js\//,
            loader: 'imports-loader?jQuery=jquery'
          },
      ]
  },
  plugins: [
    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery",
      "window.jQuery":"jquery"
    })
  ],
  resolve: {
    // https://stackoverflow.com/questions/31849476/how-to-use-blueimp-file-upload-with-webpack
    alias: {
      'load-image': 'blueimp-load-image/js/load-image.js',
      'load-image-meta': 'blueimp-load-image/js/load-image-meta.js',
      'load-image-exif': 'blueimp-load-image/js/load-image-exif.js',
      'load-image-scale': 'blueimp-load-image/js/load-image-scale.js',
      'canvas-to-blob': 'blueimp-canvas-to-blob/js/canvas-to-blob.js',
      'jquery-ui/widget': 'blueimp-file-upload/js/vendor/jquery.ui.widget.js'
    }
  },
  devServer: {
    inline: true,
    stats: { colors: true },
  }
};
