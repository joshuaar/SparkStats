import AssemblyKeys._ // put this at the top of the file                                                                                                         

assemblySettings

name := "Simple Project"

version := "1.0"

libraryDependencies ++= Seq(
  ("org.apache.spark" %% "spark-core" % "0.9.0-incubating")
)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("org", "apache", xs @ _*) => MergeStrategy.last
    case PathList("javax", "servlet", xs @ _*) => MergeStrategy.last
    case PathList("com", "esotericsoftware", xs @ _*) => MergeStrategy.last
    case PathList("project.clj") => MergeStrategy.last
    case PathList("overview.html") => MergeStrategy.last
    case PathList("about.html") => MergeStrategy.last
    case x => old(x)
  }
}

resolvers += "Akka Repository" at "http://repo.akka.io/releases/"

libraryDependencies  ++= Seq(
            "org.scalanlp" % "breeze_2.10" % "0.6-SNAPSHOT"
)

resolvers ++= Seq(
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

scalaVersion := "2.10.1"
