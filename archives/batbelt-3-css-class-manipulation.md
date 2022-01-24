title: batbelt-3-css-class-manipulation
tags: journal
date: 2016-06-23 15:31:56
---


>This is part 3 in a series of blogs detailing my attempt to write my own javascript utility library for web development.

![CSS Commit Strip](http://www.commitstrip.com/wp-content/uploads/2016/01/Strip-Tous-les-coups-sont-permis-650-finalenglish-1.jpg)



CSS Class Manipulation
---
This will involve creating part of the batbelt will mimic the `addClass`, `toggleClass` etc. methods of jQuery. These allow you to change the class attribute of a particular element.

Usage may look like this:

```javascript
cssClass.add(htmlElement, className);
cssClass.remove(htmlElement, className);
cssClass.toggle(htmlElement, className);
cssClass.has(htmlElement, className);
```

To achieve this:

  - Let's first set up the `has()` method. This method allows us to check if a particular class is already present in the class attribute. It returns a `Boolean` value to indicate this.

  ```javascript
  function has(htmlElement,className){
      var check = htmlElement.className.split(" ").filter(function (name) {
        return name === className;
      }).join();

      if (check) {
        return true;
      }

      return false;
  }
  ```

  We access the `className` property of the `Element` object. Since this read only it's pretty ok to use here because we're reading just the current value not altering it.

  __NB: I later discovered a better way of doing this using `contains` method on the `classList` object in `Element`__

  - My initial idea look like this:
    - `add` is implemented like so:
    ```javascript
    function add(htmlElement, className){
        if(!this.has(htmlElement, className)){
          htmlElement.className += " " + className ;
        }
    }
    ```
    - `remove` is implemented like so:
    ```javascript
    function remove(htmlElement, className){
      htmlElement.className = htmlElement.className.trim().split(" ").filter(function(name){
        return name !== className;
      }).join();
    }
    ```
    - `toggle`  is implemented like so:
    ```javascript
    function toggle(htmlElement,className){
        if(this.has(htmlElement,className)){
          this.remove(htmlElement,className);
        }else{
          this.add(htmlElement,className);
        }
    }
    ```
  - However I decided to go with this instead:
    - `add` is implemented like so:
    ```javascript
    function add(htmlElement, className){
        if(!this.has(htmlElement, className)){
          htmlElement.classList.add(className);
        }
    }
    ```
    - `remove` is implemented like so:
    ```javascript
    function remove(htmlElement, className){
        htmlElement.classList.remove(className);
    }
    ```
    - `toggle`  is implemented like so:
    ```javascript
    function toggle(htmlElement,className){
        htmlElement.classList.toggle(className);  
    }
    ```
    Using this I get a very simple implementation without having to touch read-only values.

  __This blog post and the [source code](https://github.com/v3rse/batbeltjs) is still evolving so check back for changes and don't hesitate to leave a comment__


Stuff I read
---
- [ClassList](http://devdocs.io/dom/element/classlist)
- [Element](http://devdocs.io/dom/element)
- [ClassName](http://devdocs.io/dom/element/classname)
