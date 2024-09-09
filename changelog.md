# doctemplates

# 0.11.0.1

  * Bump version bounds for doclayout.

  * Fix typos.

# 0.11

  * Remove HsYAML depenedency.

  * Remove upper bound for criterion.

## 0.10.0.2

  * Use doclayout 0.4.

## 0.10.0.1

  * Don't rely on aeson Object being implemented as a HashMap.
    This change is needed for doctemplates to compile against aeson 2.0.0.0.

## 0.10

  * Change rendering and conditional behavior with booleans.
    Previously, `$if(foo)$` evaluated to false iff `foo`
    would render as the empty string. This forced us to render
    a boolean False value as an empty string, rather than `false`.
    And this has caused various problems with templates
    (#16, jgm/pandoc#7402).  Now, boolean False values render as
    `false` -- just as True values render as `true`.  And conditionals
    are now sensitive to booleans, so `$if(foo)$` evaluates to false
    when `foo` is a boolean False value, even though it would render
    as the nonempty string `false`.

## 0.9

  * Add BoolVal constructor to Val.  This gives a smoother
    interface with JSON and YAML.  [API change]

  * Remove overlapping instances by generalizing
    `ToContext String String` and `FromContext String String`
    to `TemplateTarget [a] => ToContext [a] [a]` and
    `TemplateTarget [a] => FromContext [a] [a]`.
    Remove the instance `ToContext String (Doc String)`.
    Remove redundant constraints.  (#9, favonia) [API change]

## 0.8.3

  * Properly handle nested loops (#15).  Previously "it" was
    always used for the variable in a loop, and in a nested loop
    there was no way to distinguish the value of the inner
    iteration from the value of the outer one.  Now we assign
    the iterated value to both "it" and to the original variable
    name (e.g. "foo.bar").  This probably has a small negative
    performance impact.  Note that this change also affects
    the output of the template parser:  original variable
    names are now retained instead of being replaced by "it".

  * Remove duplicate IsString constraint (#14, Mario Lang).

  * Update haddocks from README (#10).

  * Minor code clean-ups (#7, favonia).

  * Add hsyaml >= 0.2 constraint (#6).


## 0.8.2

  * Add filters: first, rest, last, allbutlast.

  * New constructors for Filter: FirstItem, LastItem, Rest, AllButLast
    [API change].

## 0.8.1

  * Depend on doclayout 0.3, which adds an additional method
    on the HasChars class.  This fixes some stack overflows
    in rendering very long lines.

## 0.8

  * Change `Filter` data type to `Pipe`.  Use the nomenclature of
    "pipe" instead of "filter" to avoid confusion in pandoc between
    two notions of filter.  Otherwise everything works the same.

## 0.7.2

  * Add `nowrap` filter.

  * Improve `alpha`, `roman`, `uppercase`, `lowercase` filters so they
    apply recursively within a list or map.

  * Allow `for` loops to bind map value.  In this case there is no
    iteration, but the anophoric variable 'it' is assigned, which may
    help in using filters that destructure a string into a map (if
    we add any).

## 0.7.1

  * Add `chomp` filter.

  * Allow filters to be applied to output of partials.

## 0.7

  * Add haddock Makefile target, which regenerates haddocks from README
    and tests the code example.

  * Remove `BreakingSpace` constructor on `Template`.
    Now we use doclayout `BreakingSpace` inside a `Literal`.

  * Add instance for `ToContext a (Doc a)`.

  * Get benchmarks compiling again.

  * Use (doclayout) `Doc` internally and for rendered output.

    + `TemplateTarget` is now a type constraint synonym, not a regular
      typeclass.
    + Constraint on `compileTemplate` and `applyTemplate`
      simplified using TemplateTarget.
    + DocTemplates reexports Text.DocLayout.Doc.
    + The `Literal` costructor of `Template` now takes a `Doc a`
      rather than an `a`.
    + The `SimpleVal` constructor of `Val` now takes a `Doc a`
      rather than an `a`.
    + `renderTemplate` now returns a `Doc a` rather than an `a`.
      (This value can be converted to an a using `render Nothing`.)

  * Remove fromText from `TemplateTarget`.  Now we use `fromString`
    from Data.String.

  * Parameterize `Template` on underlying stringlike type.

  * Improved behavior of partials.

  * Improve indent functions: don't drop final newline.

  * Allow blank lines in nested section.

  * Indent for Text/String: don't indent empty lines.

  * Additional tests and documentation about nesting.

  * Render items in for loop before separator.
    Otherwise we throw off column calculation.

  * Remove `+-reflow`; replace with toggle `$~$`.

  * Remove pNewline parser; it isn't needed now.

  * Remove `+-nest`.

  * Fix nest parsing bug.

  * Improve nesting.

    + Change `Nested` constructor for `Template` so it doesn't take
      a parameter.
    + Nesting level is now determined dynamically at render time
      rather than at compile time.  This gives much better results
      when nesting occurs after template directives.
      Benchmarks show a slight penalty in performance (from 3.5ms to 3.1ms
      in rendering), but it's not too much.

  * Add filters. Filters transform the value of a variable, e.g. changing
    a map into an array of key/value pairs.  Closes #5.

    + Internal: Add `Filter` type and `[Filter]` parameter on `Variable`.
    + Remove `unVariable`; now we have `varParts` and `varFilters`.
    + Document filters in README.md.
    + Implement filters.
    + Add tests.

  * Add `ToYAML`, `FromYAML` instances for `Context`, `Val`.

## 0.6.2

  * Remove unnecessary `TemplateTarget` constraints on
    `ToContext` instances.

  * Add `ToContext` instance for `Map Text a`.

  * Add `Data`, `Typeable` instances for `Context` and `Val`.

## 0.6.1

  * Indent bare partials.

## 0.6

  * Add `+nest`/`-nest` keywords.

  * Add `+reflow`/`-reflow` keywords.

  * Add Nested constructor to Template, remove Indented
    and Indented parameter for Interpolate.

  * More expansive description of library.

## 0.5.1

  * Add elseif keyword.

  * Improve compile error source locations with partials.

  * Handle templates that don't end in newlines.  Previously
    this caused problems in some cases.

## 0.5

  * Add toText method to TemplateTarget class.

  * Add String and Lazy Text instances for TemplateTarget.

  * Swap Parameters in ToContext (so that the first parameter
    for both ToContext and FromContext refers to the parameter
    of Context).

  * Add toVal method to ToContext.

  * Default instance definition for toContext in terms of toVal,
    so that defining toVal is sufficient.

  * Add instances for ToContext and FromContext.

  * Remove valueToContext. Add ToJSON, FromJSON instances
    for Context and Val instead.

  * isEmpty: For Doc, treat `Text 0 _` as empty.
    Also `Concat x y` when x and y are empty.
    This differs from isEmpty in DocLayout itself, which only
    applies to Empty.

  * Code cleanup.

## 0.4

  * Split into three modules.  Main module only exports an
    opaque version of the Template type.  Import Internal if you
    need to manipulate a Template.

  * Add Context type, parameterized on the underlying content's type.

  * Add Val type.

  * Add valueToContext for converting an Aeson Value to a Context.

  * Make renderTemplate and applyTemplate polymorphic in both
    context and target.  Context parameter is now any instance
    of ToContext (instead of ToJSON).  Result is now any
    instance of TemplateTarget.

  * Change type of getPartial in TemplateMonad so it runs in the
    TemplateMonad instance, not the Parser.  Return a simple
    value rather than an Either; error handling can vary with
    the monad.

  * Remove TemplatePart. Template is now an algebraci data type,
    not a list of TemplateParts.

  * Add an Indented type to indicate indentation for
    interpolated variables.

  * Improve architecture, doing more at compile time.

  * Depend on doclayout.  Context can be parameterized on a doclayout
    Doc type, allowing intelligent reflowing of content.

  * Remove single final newline in interpolated variable.

  * Remove final newline from partial.

  * Don't iterate when the variable evaluates to NullVal.

  * Only indented interpolated variables if by themselves on line.

  * Add Indented parameter to Interpolate constructor.

  * Update documentation and haddocks.

  * Add benchmark.

## 0.3.0.1

* Bump lower bound on base to 4.9, drop support for ghc 7.10.

* Add needed import for older base versions.

* Add test.hs to repository.

## 0.3

* Note that all of the changes to template syntax described
  below are backwards compatible, and all old pandoc templates
  should continue to work as before.

* Allow `${...}` style delimiters around variables and
  directives, in addition to `$...$`.  Allow space around
  the delimiters.

* Support `$it$` as a variable for the current value in
  an iteration.  (The old method, where the containing
  variable name is used, still works.)

* Support partials (subtemplates defined in different files).

* Interpolated array variables now have all elements rendered,
  concatenated, with an optional separator that can be
  specified using a new bracketed syntax.

* Remove `TemplateTarget` class.  It was pointless; the
  calling program can just do these trivial transformations.
  Avoids dependencies on bytestring, blaze-html, blaze-markup.

* Change type of `renderTemplate` and `applyTemplate` to produce
  a `Text`, instead of being polymorphic.

* Changed type of `compileTemplate`: it now takes a
  template path and the template contents, and returns
  either a template or an error.  It runs in an instance
  of `TemplateMonad`, which is an abstraction around different
  ways of getting partials.  (For example, in IO we can get
  partials by reading them from a file system, but in a
  web application one might want to obtain them from the
  database or have a set of them baked in.)

* Remove `varListToJSON`.

* Changed the architecture: `Template` is no longer just
  a newtype around a function, but a list of `TemplatePart`s.

* Added a newtype for `Variable`.

* Improved documentation in README.md.

* Added a new test framework and much more extensive tests.

