val sangriaVersion        = "4.2.20" // pinned 4.x: a 5.x bump is a migration decision, not a dependabot merge
val catsVersion           = "2.13.0"
val catsEffectVersion     = "3.7.1"
val circeVersion          = "0.14.16"
val hedgehogsVersion      = "0.5.0"
val http4sVersion         = "0.23.30"
val munitVersion          = "1.3.6"
val calibanVersion        = "2.11.2" // for the side-by-side comparison example only
val zioInteropCatsVersion = "23.1.0.14"

ThisBuild / scalaVersion  := "3.3.7"
ThisBuild / organization  := "net.andimiller"
ThisBuild / version       := "0.1.0-SNAPSHOT"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / licenses      := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")
)

// No parser: plan data and the executor only, so this module can cross-build for JS later.
lazy val core = (project in file("modules/core"))
  .settings(commonSettings)
  .settings(
    name := "torpor-core",
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-core"             % catsVersion,
      "org.typelevel"  %% "cats-effect"           % catsEffectVersion,
      "io.circe"       %% "circe-core"            % circeVersion,
      "net.andimiller" %% "hedgehogs-dag-visitor" % hedgehogsVersion,
      "org.scalameta"  %% "munit"                 % munitVersion % Test
    )
  )

lazy val schema = (project in file("modules/schema"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "torpor-schema",
    libraryDependencies ++= Seq(
      "org.sangria-graphql" %% "sangria-ast"    % sangriaVersion,
      "org.sangria-graphql" %% "sangria-parser" % sangriaVersion,
      "org.typelevel"       %% "cats-core"      % catsVersion,
      "org.scalameta"       %% "munit"          % munitVersion % Test
    )
  )

// Compile-time checks. Separate module because a macro cannot be used in the compilation run that defines it.
lazy val macros = (project in file("modules/macros"))
  .dependsOn(schema)
  .settings(commonSettings)
  .settings(
    name                                   := "torpor-macros",
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
  )

// Golden SDL: the merged contract as one reviewable .graphqls file, checked by a diff on every run.
lazy val testkit = (project in file("modules/testkit"))
  .dependsOn(core, schema)
  .settings(commonSettings)
  .settings(
    name := "torpor-testkit",
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion
  )

lazy val source = (project in file("modules/source"))
  .dependsOn(core, schema, snapshot, macros % Test)
  .settings(commonSettings)
  .settings(
    name := "torpor-source",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"         % catsVersion,
      "io.circe"      %% "circe-parser"      % circeVersion,
      "org.typelevel" %% "munit-cats-effect" % "2.2.1"      % Test,
      "org.scalameta" %% "munit"             % munitVersion % Test
    )
  )

lazy val snapshot = (project in file("modules/snapshot"))
  .dependsOn(core, schema)
  .settings(commonSettings)
  .settings(
    name := "torpor-snapshot",
    libraryDependencies ++= Seq(
      "net.andimiller" %% "hedgehogs-dag-visitor-circe" % hedgehogsVersion,
      "org.tpolecat"   %% "skunk-core"                  % "1.0.0",
      "io.circe"       %% "circe-parser"                % circeVersion,
      "org.scalameta"  %% "munit"                       % munitVersion % Test
    )
  )

// M2: what the plan will do, before it does it.
lazy val explain = (project in file("modules/explain"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name                                   := "torpor-explain",
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
  )

lazy val http = (project in file("modules/http"))
  .dependsOn(core, schema, source, macros % Test)
  .settings(commonSettings)
  .settings(
    name := "torpor-http",
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-core"       % http4sVersion,
      "org.http4s"    %% "http4s-dsl"        % http4sVersion,
      "org.http4s"    %% "http4s-circe"      % http4sVersion,
      "org.typelevel" %% "munit-cats-effect" % "2.2.1"      % Test,
      "org.scalameta" %% "munit"             % munitVersion % Test
    )
  )

lazy val conformance = (project in file("modules/conformance"))
  .dependsOn(schema)
  .settings(commonSettings)
  .settings(
    name                                   := "torpor-conformance",
    publish / skip                         := true,
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
  )

// Example: EVE Online's ESI, unauthenticated, with the required custom User-Agent.
// Shared by both ESI examples: config, wire types and the HTTP client. Nothing GraphQL in here.
lazy val examplesSharedEsi = (project in file("examples/shared-esi"))
  .settings(commonSettings)
  .settings(
    name           := "torpor-examples-shared-esi",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-ember-client" % http4sVersion,
      "org.http4s"    %% "http4s-circe"        % http4sVersion,
      "io.circe"      %% "circe-core"          % circeVersion,
      "org.typelevel" %% "cats-effect"         % catsEffectVersion
    )
  )

lazy val esi = (project in file("examples/esi"))
  .dependsOn(core, schema, source, http, macros, explain, examplesSharedEsi)
  .settings(commonSettings)
  .settings(
    name           := "torpor-example-esi",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-ember-client" % http4sVersion,
      "org.http4s"    %% "http4s-ember-server" % http4sVersion,
      "org.http4s"    %% "http4s-circe"        % http4sVersion,
      "org.typelevel" %% "munit-cats-effect"   % "2.2.1"      % Test,
      "org.scalameta" %% "munit"               % munitVersion % Test
    )
  )
  .dependsOn(testkit % Test)

// Same ESI example, built with Caliban: standalone, so it can be read and diffed against `examples/esi` directly.
lazy val esiCaliban = (project in file("examples/esi-caliban"))
  .dependsOn(examplesSharedEsi)
  .settings(commonSettings)
  .settings(
    name           := "torpor-example-esi-caliban",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban"             % calibanVersion,
      "com.github.ghostdogpr" %% "caliban-http4s"      % calibanVersion,
      "dev.zio"               %% "zio-interop-cats"    % zioInteropCatsVersion,
      "org.http4s"            %% "http4s-ember-client" % http4sVersion,
      "org.http4s"            %% "http4s-ember-server" % http4sVersion,
      "org.http4s"            %% "http4s-circe"        % http4sVersion,
      "org.scalameta"         %% "munit"               % munitVersion % Test
    )
  )

// README.md is generated: edit doc-src/README.md, then `sbt docs/mdoc`.
lazy val docs = (project in file("docs-mdoc"))
  .dependsOn(source, macros, http, examplesSharedEsi)
  .enablePlugins(MdocPlugin)
  .settings(commonSettings)
  .settings(
    name           := "torpor-docs",
    publish / skip := true,
    mdocIn         := (ThisBuild / baseDirectory).value / "doc-src",
    libraryDependencies += "org.http4s" %% "http4s-ember-server" % http4sVersion,
    mdocOut        := (ThisBuild / baseDirectory).value
  )

lazy val root = (project in file("."))
  .aggregate(core, schema, macros, source, snapshot, explain, http, conformance, testkit, examplesSharedEsi, esi, esiCaliban)
  .settings(
    name           := "torpor",
    publish / skip := true
  )
