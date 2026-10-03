plugins {
    kotlin("jvm") version "2.4.0"
}

repositories {
    mavenCentral()
    mavenLocal()
}

val leanBin = System.getenv("LEAN_BIN")?.let { "$it/lean" }
    ?: project.findProperty("leanBin")?.toString()
    ?: listOf(
        rootProject.file("../../build/release/stage1/bin/lean").absolutePath,
        rootProject.file("../build/release/stage1/bin/lean").absolutePath
    ).firstOrNull { File(it).exists() } ?: "lean"

val generatedKotlinDir = layout.buildDirectory.dir("generated/sources/lean/kotlin/main")

val generateLean by tasks.registering {
    inputs.files(fileTree(projectDir) {
        include("*.lean")
        include("*.lean.init.sh")
    })
    if (File(leanBin).exists()) {
        inputs.file(leanBin)
        val leanShared = File(File(leanBin).parentFile, "../lib/lean/libleanshared.so")
        if (leanShared.exists()) {
            inputs.file(leanShared)
        }
    }
    outputs.dir(generatedKotlinDir)

    doLast {
        val outDir = generatedKotlinDir.get().asFile
        outDir.mkdirs()

        val leanFiles = projectDir.listFiles { file ->
            file.extension == "lean" && file.name != "ownership_fail.lean"
        } ?: emptyArray()

        for (leanFile in leanFiles) {
            val baseName = leanFile.nameWithoutExtension
            val ktOut = File(outDir, "$baseName.kt")
            val initFile = File(projectDir, "$baseName.lean.init.sh")

            val script = """
                args=()
                if [ -f "${initFile.absolutePath}" ]; then
                    source "${initFile.absolutePath}"
                    args=(${'$'}{TEST_LEAN_ARGS[@]+"${'$'}{TEST_LEAN_ARGS[@]}"})
                fi
                "$leanBin" -K "${ktOut.absolutePath}" -Dcompiler.postponeCompile=false -Dcompiler.kotlin.package=tests.kotlin ${'$'}{args[@]+"${'$'}{args[@]}"} "${leanFile.absolutePath}"
            """.trimIndent()

            val proc = ProcessBuilder("bash", "-c", script)
                .redirectErrorStream(true)
                .start()
            val output = proc.inputStream.bufferedReader().readText()
            val exitCode = proc.waitFor()
            if (exitCode != 0) {
                throw GradleException("Failed to generate Kotlin for ${leanFile.name}: $output")
            }
        }
    }
}

sourceSets {
    main {
        kotlin {
            srcDir(generateLean)
        }
    }
}

dependencies {
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}
