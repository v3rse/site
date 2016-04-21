title: Learn React - Stage 2 - Learn Webpack (and about CommonJS)
tags: journal
---
![webpack](https://cdn-images-1.medium.com/max/2000/1*A-_KrEvMuiH7dlwshFw5aw.png)

![image](https://camo.githubusercontent.com/cecf339daf48ee4cfe86f33630c3732cdc2ab0e1/68747470733a2f2f696d67732e786b63642e636f6d2f636f6d6963732f6578706c6f6974735f6f665f615f6d6f6d2e706e67)

Stuff I read on CommonJS
---

- [0FPS - CommonJS: Why and How](http://0fps.net/2013/01/22/commonjs-why-and-how/)
- [node aesthetic by substack](http://substack.net/node_aesthetic)

The reference I'm using can be found [here](https://github.com/petehunt/webpack-howto)

Another group references are the [webpack tutorial](http://webpack.github.io/docs/tutorials/getting-started/) and [webpack docs](http://webpack.github.io/docs/)

Sample file structure  [here](https://github.com/petehunt/react-webpack-template).

Steps
---
-



Notes
---
- It's like browserify but can split your app into multiple files. If you have multiple pages in a single-page app, the user only downloads code for just that page. If they go to another page, they don't redownload common code.
```
browserify main.js > bundle.js
```
is equal to
```
webpack main.js bundle.js
```
- It often replaces grunt or gulp because it can build and bundle CSS, preprocessed CSS, compile-to-JS languages and images, among other things.
- It supports AMD and CommonJS, among other module systems (Angular, ES6)
- webpack allows you to have sort of like build file called `webpack.config.js` where you export an object configuring how it should work in you project
- Real code can be used in config file!!
- webpack allows you to use modules like __loaders__. Popular loaders are __coffee-loader__ and __babel-loader__ which allows you to compile  __CoffeeScript__ and __JSX__(and __ES2015__) code respectively to JavaScript using different presets available on npm.
eg.
```
// webpack.config.js
module.exports = {
  entry: './main.js',
  output: {
    filename: 'bundle.js'
  },
  module: {
    loaders: [
      { test: /\.coffee$/, loader: 'coffee-loader' },
      {
        test: /\.js$/,
        loader: 'babel-loader',
        query: {
          presets: ['es2015', 'react']
        }
      }
    ]
  }
};
```
- To require files in webpack with weird extensions without adding the extensions just add `resolve.extensions` field in your config object.
```
resolve: {
    // you can now require('file') instead of require('file.coffee')
    extensions: ['', '.js', '.json', '.coffee']
  }
```
- To package static content like CSS and images you can use `require`!!

  ```
  require('./bootstrap.css');
  require('./myapp.less');

  var img = document.createElement('img');
  img.src = require('./glyph.png');
  ```

- When you require CSS (or less, etc), webpack inlines the CSS as a string inside the JS bundle and `require()` will insert a `<style>` tag into the page.
- When you require images, webpack inlines a URL to the image into the bundle and returns it from `require()`.
- But loaders make everything better. Checkout the path, publicPath and image loader!!:
```
module.exports = {
  entry: './main.js',
  output: {
    path: './build', // This is where images AND js will go
    publicPath: 'http://mycdn.com/', // This is used to generate URLs to e.g. images
    filename: 'bundle.js'
  },
  module: {
    loaders: [
      { test: /\.less$/, loader: 'style-loader!css-loader!less-loader' }, // use ! to chain loaders
      { test: /\.css$/, loader: 'style-loader!css-loader' },
      { test: /\.(png|jpg)$/, loader: 'url-loader?limit=8192' } // inline base64 URLs for <=8k images, direct URLs for the rest
    ]
  }
};
```
- Feature flag can be used to filter out feature depending on build parameters since webpack uses uglify to strip out dead-code.
```
//in code
if (__DEV__) {
  console.warn('Extra logging');
}
// ...
if (__PRERELEASE__) {
  showSecretFeature();
}
```
And then teach webpack:

  ```
  // webpack.config.js

  // definePlugin takes raw strings and inserts them, so you can put strings of JS if you want.
  var definePlugin = new webpack.DefinePlugin({
    __DEV__: JSON.stringify(JSON.parse(process.env.BUILD_DEV || 'true')),
    __PRERELEASE__: JSON.stringify(JSON.parse(process.env.BUILD_PRERELEASE || 'false'))
  });

  module.exports = {
    entry: './main.js',
    output: {
      filename: 'bundle.js'
    },
    plugins: [definePlugin]
  };
  ```
- Multiple entry points can be specified for you app to generate multiple bundle files.  

```
// webpack.config.js
module.exports = {
  entry: {
    Profile: './profile.js',
    Feed: './feed.js'
  },
  output: {
    path: 'build',
    filename: '[name].js' // Template based on keys in entry above
  }
};
```

```
<script src="build/Profile.js"></script>
```

- Caching for common code between these 2 different bundles is easily done:

```
// webpack.config.js

var webpack = require('webpack');

var commonsPlugin =
  new webpack.optimize.CommonsChunkPlugin('common.js');

  module.exports = {
  entry:{..}
  output:{..}
  plugins: [commonsPlugin]

  };

```
Then use  common.js in a script tag before your bundle.
-Async loading is possible but I don't get it.
