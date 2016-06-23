title: Building batbelt.js -Part 2 - CSS Manipulation and Testing
tags: journal
date: 2016-05-26 02:14:18
---


>This is part 2 in a series of blogs detailing my attempt to write my own javascript utility library for web development.

![CSS Commit Strip](http://www.commitstrip.com/wp-content/uploads/2016/05/Strip-The-4-th-of-may-650-finalenglsih-1.jpg)

First thing I'll look at is the testing library. I'll be using `Jasmine` because I've had some experience with it and comes with an assertion library and a couple of things `Mocha` doesn't come prepackaged with.

There's also a `webpack` Jasmine [plugin](https://www.npmjs.com/package/jasmine-webpack-plugin). I won't be using that for now. Maybe later. I'm simply going to include the necessary script tags in my `index.html` to run the tests. This will endure that all you need to do to run the tests is to run webpack and open the `index.html` file. You, however, need to reload the file on code change. I may change that later as well.

Test Structure
---
![Jasmine](http://jasmine.github.io/images/jasmine_vertical.png)

- All testing dependencies like `jasmine.js`,`jasmine-html.js`,`jasmine/boot.js` and `jquery-2.2.4.min.js`(yes I know) are placed in the `index.html file`

```html
<!-- dependecies -->
<script src="lib/jasmine/jasmine.js" charset="utf-8"></script>
<script src="lib/jasmine/jasmine-html.js" charset="utf-8"></script>
<script src="lib/jasmine/boot.js" charset="utf-8"></script>
<script src="lib/jquery-2.2.4.min.js" charset="utf-8"></script>


<script src="./dist/batbelt.main.js"></script>
<script src="./dist/batbelt.test.js"></script>
```

- Test files can be found in their corresponding feature directories under `test`. The corresponding source file is required using CommonJS style importing.

```javascript
module.exports = function (){
  return "Hello Tests!";
};
```


```javascript
var css = require('../../src/css/css');
describe('css', function() {
  it('should return "Hello Tests!"',function () {
    expect(css()).toBe('Hello Tests!');
  });
});
```

- To build a the `main` file for use in browser I borrowed the method used in jQuery. I created function called `batbelt` and then attach it to the window object. This function returns a little object with the feature methods. Yes I know this is not completely __Vanilla Js__ (sue me! :wink:) but it helps me modularize my code.

```javascript
var css = require('./css/css');

function batbelt() {
  return {
    css:css
  };
}

window.batbelt = batbelt;
```
__NB: Mimicked the test suite from  [leanQuery](https://github.com/infinum/learnQuery)__

```javascript
describe('css', function() {
  'use strict';

  beforeEach(function() {
    //create some dummy elements to test with
    //using jasmine-fixture
    affix('#toy .book');

    //jquery version of selection
    this.$toy = $('#toy');
    this.toy = this.$toy[0];
  });

  it('should set a CSS attribute of an element',function () {
    css(this.toy, 'width', '500px');

    //Verify using jQuery's version
    expect(this.$toy.css('width')).toBe('500px');
  });
```

Using the `jasmine-fixture` [library](https://github.com/searls/jasmine-fixture) I can `affix` some elements into the dom to play with during testing


CSS Manipulation
---
The function should take in an element and a key  and value or  just an object as arguments and change the style style attribute on the element corresponding to the key. Depending on the arguments passed it may functionality will change.

Usage may look like this:
```
css(toy, 'width', '9001px')
css(this.toy, {
      'height': '100px',
      'display': 'none'
    });
css(toy, 'display')
```

To achieve this:

- I would need to do some function overloading using `if` statements, the `arguments` object available to JavaScript functions and the `typeof` operator:
  ```javascript
  if (arguments.length > 1) {//check number of arguments
    if(typeof element === 'undefined'){
    // ....
    }
    if (typeof key === 'string') {//if second argument is a string
      if (typeof value !== 'undefined') {//and third is defined
      //...
      }else {//if third is undefined
      //...
      }
    }else if( typeof key === 'object'){//if second argument is an object
        //set multiple properties
        //....
      }
  }else {//if fewer than one element is set then through error.
    console.error('Provide the necessary arguments');
  }
  ```
- With the overloading implemented I'll need a way to access the styling on the element. [MDN](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/style) says:
    - use the properties of the `style` properties of the `HTMLElement` object to set your style values.
    - use `window.getComputedStyle` to retrieve an accurate value for your elements style.


  __This blog post and the [source code](https://github.com/v3rse/batbeltjs) is still evolving so check back for changes and don't hesitate to leave a comment__


Stuff I read
---
- [Mocha vs. Jasmine](http://thejsguy.com/2015/01/12/jasmine-vs-mocha-chai-and-sinon.html)
- [Multiple Entry Point](https://github.com/webpack/webpack/tree/master/examples/multiple-entry-points)
- [Jasmine Intro](http://jasmine.github.io/2.4/introduction.html)
- [Jasmine Fixtures](https://github.com/searls/jasmine-fixture)


Notes
---
- Need to run `npm init`. Totally forgot
- Had to install `css-loader` and `style-loader` separately for webpack to load css.
- Placing `jasmine` and `jquery` in lib directory
- Had to use multiple entry points to get two bundles. One for test and one for main library
- Jasmine uses `this` to share context within a spec.
