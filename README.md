# `defcss`
Painless CSS in Lisp

You can think of it as something like less.js but you don't need NodeJS and javascript
to use it. It is Lisp.

Under the hood, it uses [LASS](https://github.com/Shinmera/LASS) to generate CSS, while ensuring
class names, and at the same time letting you redefine CSS at the REPL and develop
as you usually develop Common Lisp.

## Tutorial
Let's begin with a simple class

```lisp
(defcss primary-background
  :background-color "#ffffff")
```

with `defcss`, you can compose classes with other classes to create new classes. It is a lot
like CLOS mixins.
```lisp
(defcss (button primary-background)
  :text-color "#222222")
```

You can compose multiple classes and even override existing styles!
```lisp
(defcss (error-button error-background))
;; A body is optional!
```

Of course, this creates the question, how can I use these CSS definition from a web server?

First, in the HTML, you can use `style-class` to get the actual class name
```lisp
;; Just printing it out
(style-class button) ; => button2
```

Notice how the class names are still human readable. `defcss` tries to generate something that
can be debugged by a human.

Of course, just having the class name is useless. You need a file to store everything.

For this, you can use `file`s.
```lisp
(defvar *index.css* (make-file))
```

Then, for every style that needs to be included, just add the `:in` option to include it in
a particular file
```lisp
(defcss (button primary-background (:in *index.css*))
  :text-color "#222222")
```

Finally, to generate a css file, use `file->string`
```lisp
(file->string *index.css*) ; => ".button4{ background-color: #ffffff; text-color: #222222; }"
```
