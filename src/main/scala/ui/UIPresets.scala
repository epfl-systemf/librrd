package ui

object UIPresets:
  val diagramPresets = Map(
    "SQLite create-table" ->
      """("CREATE"
        | (+ (+ () "TEMP") "TEMPORARY") "TABLE"
        | (+ ("IF" "NOT" "EXISTS") ())
        | (+ ("schema-name" ".") ()) "table-name"
        | (+ ("AS" [select-stmt])
        |    ("(" (- [column-def] ",")
        |         (- () ("," [table-constraint])) ")"
        |         (+ () [table-options]))))""".stripMargin,
    "SQLite table-constraint" ->
      """((+ ("CONSTRAINT" "NAME") ())
        | (+ ((+ ("PRIMARY" "KEY") "UNIQUE")
        |     {"(" class="punct"}
        |     (- [indexed-column] {"," class="punct"})
        |     {")" class="punct"}
        |     [conflict-clause])
        |    (+ ("CHECK"
        |        {"(" class="punct"} [expr] {")" class="punct"})
        |       ("FOREIGN" "KEY"
        |        {"(" class="punct"}
        |        (- "column-name" {"," class="punct"})
        |        {")" class="punct"}
        |        [foreign-key-clause]))))""".stripMargin,
    "SQLite compound-select" ->
      """((+ ("WITH" (+ () "RECURSIVE") (- [common-table-expression] ",")) ())
        | (- [select-core] (+ (+ "UNION" ("UNION" "ALL")) (+ "INTERSECT" "EXCEPT")))
        | (+ ("ORDER" "BY" (- [ordering-term] ",")) ())
        | (+ ("LIMIT" [expr] (+ () (+ ("OFFSET" [expr]) ("," [expr])))) ()))""".stripMargin,
    "JSON number" ->
      """((+ () "-")
        | (+ "0" ([digit 1-9] (- () [digit])))
        | (+ () ("." (- [digit] ())))
        | (+ ()
        |    ({(+ "E" "e") class="bottom-aligned"}
        |     {(+ (+ "-" ()) "+") class="center-aligned"}
        |     (- [digit] ()))))""".stripMargin,
    "JSON array" -> """("[" (+ [whitespace] (- [value] ",")) "]")""",
  )

  val layoutPresets = Map(
    "SQLite" ->
      """:root {
        |    align-items: top;
        |    justify-content: space-between;
        |}
        |
        |* {
        |    flex-absorb: 0.3;
        |}
        |
        |.punct {
        |    font: sans-serif normal bold;
        |}""".stripMargin,
    "JSON" ->
      """.bottom-aligned { align-self: bottom; }
        |.center-aligned { align-self: center; }
        |
        |:root {
        |  justify-content: space-evenly;
        |}
        |
        |nonterminal {
        |  font: 'Gill Sans' normal bold 1.4em;
        |}
        |
        |terminal {
        |  font: Inconsolata normal normal 1.4em;
        |}""".stripMargin,
  )

  val renderingPresets = Map(
    "SQLite" ->
      """.librrd-rail, .librrd-station rect, path {
        |    stroke-width: 1px;
        |    stroke: black;
        |    fill: none;
        |}
        |
        |rect.librrd-group {
        |    fill: none;
        |    stroke: none;
        |}
        |
        |.librrd-selected > rect.librrd-group {
        |    fill: hsl(0 0% 75% / 0.5);
        |    stroke: hsl(0 0% 50%);
        |    stroke-width: 2px;
        |    stroke-dasharray: 4 2;
        |}""".stripMargin,

    "JSON" ->
      """.librrd-rail, .librrd-station rect, path {
        |    stroke-width: 3px;
        |    stroke: black;
        |    fill: none;
        |}
        |
        |.librrd-station rect {
        |    stroke-width: 2px;
        |}
        |
        |rect.librrd-group {
        |    fill: none;
        |    stroke: none;
        |}
        |
        |.librrd-selected > rect.librrd-group {
        |    fill: hsl(0 0% 75% / 0.5);
        |    stroke: hsl(0 0% 50%);
        |    stroke-width: 2px;
        |    stroke-dasharray: 4 2;
        |}""".stripMargin,
  )
