plugins {
    kotlin("multiplatform") version "2.0.21"
}

group = "org.leanprover"
version = "4.0.0"

kotlin {
    jvm {
        compilerOptions {
            jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_17)
            freeCompilerArgs.add("-Xjsr305=strict")
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
