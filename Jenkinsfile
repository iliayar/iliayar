pipeline {
    agent any

    options {
	githubProjectUrl("https://github.com/iliayar/iliayar.git")
    }

    triggers {
	gitHub()
    }

    stages {
	stage("Test") {
	    steps {
		script {
		    sh "echo AYAYAYAYAYY"
		}
	    }
	}
    }
}
