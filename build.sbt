libraryDependencies ++= Seq(
	compilerPlugin("org.scala-lang.plugins" % ("scala-continuations-plugin_" + scalaVersion.value) % "1.0.2")
)

scalaSource in Compile <<= baseDirectory(_ / "src")

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

autoCompilerPlugins := true

scalacOptions += "-P:continuations:enable"

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")
