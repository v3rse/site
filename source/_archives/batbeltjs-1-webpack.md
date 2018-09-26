title: Building batbelt.js -Part 1 - Setting up the dev environment
tags: journal
date: 2016-05-25 01:14:56
---

Why webpack?
---
![image](https://imgs.xkcd.com/comics/tools.png)

I honestly don't hav a good reason. I've heard the hype and I'm just trying it out. It has some pretty cool features. It may seem like a bit of overkill looking at the features but hey, I'm try stuff out.


![webpack](https://cdn-images-1.medium.com/max/2000/1*A-_KrEvMuiH7dlwshFw5aw.png)

Stuff I read on CommonJS
---
- [0FPS - CommonJS: Why and How](http://0fps.net/2013/01/22/commonjs-why-and-how/)
- [node aesthetic by substack](http://substack.net/node_aesthetic)

The reference I'm using can be found [here](https://github.com/petehunt/webpack-howto)

Another group references are the [webpack tutorial](http://webpack.github.io/docs/tutorials/getting-started/) and [webpack docs](http://webpack.github.io/docs/)

Steps
---

__Setting up the directory structure__

I decided to go with something Maven-esque

```
batbeltjs
├── dist
│   └── batbelt.js
├── index.html
├── LICENSE
├── README.md
├── src
│   └── main.js
├── test
└── webpack.config.js

```

__Write config file__

I'm guessing I might be using a little ES2015 sugar so I'll be using `babel` in my webpack config.

```sh
npm install babel-core babel-loader jshint jshint-loader babel-preset-es2015 webpack --save-dev
```

This the file I ended up with

```javascript
module.exports = {
  entry: './src/main.js',
  output: {
    filename: './dist/batbelt.js'
  },
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: 'jshint-loader'
      }
    ],
    loaders: [
      {
        test: /\.js$/,
        loader: 'babel-loader',
        exclude: /node_modules/,
        query: {
          presets: ['es2015']
        }
      }
    ]
  },
  resolve: {
   extensions: ['','.js']
 },
  watch: true
};

```

I tested this setup with the `.html` file in the file structure above. This may later be used with jasmine test...maybe.



Notes
---
- It's like browserify but can split your app into multiple files. If you have multiple pages in a single-page app, the user only downloads code for just that page. If they go to another page, they don't redownload common code.
```sh
browserify main.js > bundle.js
```
is equal to
```sh
webpack main.js bundle.js
```
- It often replaces grunt or gulp because it can build and bundle CSS, preprocessed CSS, compile-to-JS languages and images, among other things.
- It supports AMD and CommonJS, among other module systems (Angular, ES6)
- webpack allows you to have sort of like build file called `webpack.config.js` where you export an object configuring how it should work in you project
- Real code can be used in config file!!
- webpack allows you to use modules like __loaders__. Popular loaders are __coffee-loader__ and __babel-loader__ which allows you to compile  __CoffeeScript__ and __JSX__(and __ES2015__) code respectively to JavaScript using different presets available on npm.
eg.
```javascript
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
```javascript
resolve: {
    // you can now require('file') instead of require('file.coffee')
    extensions: ['', '.js', '.json', '.coffee']
  }
```
- To package static content like CSS and images you can use `require`!!

  ```javascript
  require('./bootstrap.css');
  require('./myapp.less');

  var img = document.createElement('img');
  img.src = require('./glyph.png');
  ```

- When you require CSS (or less, etc), webpack inlines the CSS as a string inside the JS bundle and `require()` will insert a `<style>` tag into the page.
- When you require images, webpack inlines a URL to the image into the bundle and returns it from `require()`.
- But loaders make everything better. Checkout the path, publicPath and image loader!!:
```javascript
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
```javascript
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

  ```javascript
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
```javascript
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

  ```html
  <script src="build/Profile.js"></script>
  ```

- Caching for common code between these 2 different bundles is easily done:

  ```javascript
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
