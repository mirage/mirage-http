### mirage-http -- Http abstraction and implementations for MirageOS

This is a work in progress, we need to find a good interface that abstracts and unify Cohttp and Httpaf libraries.
It's split into three subpackages:

* `mirage-http`: HTTP interface
* `mirage-http.cohttp`: Cohttp wrapper
* `mirage-http.httpaf`: Httpaf wrapper

### Httpaf

It doesn't work on upstream httpaf, it needs features that are pending for review:
- Mirage adapter (https://github.com/inhabitedtype/httpaf/pull/83)
- Upgrade connection (https://github.com/inhabitedtype/httpaf/pull/91)

### Examples

You can take a look in `test/` for a sample of client/server that use the abstraction.
