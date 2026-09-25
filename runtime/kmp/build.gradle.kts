plugins {
    kotlin("multiplatform") version "2.4.0"
    `maven-publish`
}

group = "org.leanprover"
version = "4.0.0"

repositories {
    mavenCentral()
    mavenLocal()
}

kotlin {
    jvm {
        compilerOptions {
            jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_17)
            freeCompilerArgs.add("-Xjsr305=strict")
            freeCompilerArgs.add("-Xexpect-actual-classes")
        }
    }

    // Ready for multiplatform targets
    // js { browser(); nodejs() }
    // wasmJs { browser() }
    // linuxX64(); macosArm64(); mingwX64()

    sourceSets {
        commonMain.dependencies {
            // Self-contained runtime: no external dependencies
        }
        commonTest.dependencies {
            implementation(kotlin("test"))
        }
        jvmMain.dependencies {
            // JVM-specific dependencies
        }
    }
}

val standaloneRuntimeJar by tasks.registering(Jar::class) {
    archiveBaseName.set("lean-runtime")
    archiveClassifier.set("")
    archiveVersion.set("")
    from(kotlin.jvm().compilations.getByName("main").output)
    from({
        configurations.getByName("jvmRuntimeClasspath").map { file ->
            if (file.isDirectory) file else zipTree(file)
        }
    })
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}

