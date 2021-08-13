pipeline {
    agent any

    triggers {
	githubPush()
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
