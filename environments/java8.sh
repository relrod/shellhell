function generate {
    PATH=/usr/lib/jvm/java-1.8.0-openjdk.x86_64/bin/:$PATH
    sbt -java-home /usr/lib/jvm/java-1.8.0-openjdk.x86_64/ doc
}
