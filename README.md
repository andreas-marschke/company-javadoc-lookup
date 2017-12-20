## company-javadoc-lookup

> company-mode completion backend for javadoc-lookup

### Description

A company-mode backend adding support for javadoc-lookup where we are resolving based
on classes accessible via `jdl/get-class-list'

### Features

- supports basic lookup based on javadoc-lookup
- Completes from root namespaces down to classes

### Installation

Add add a hook to the java-mode-hook:

```lisp
(add-hook 'java-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-javadoc-lookup)))
```
