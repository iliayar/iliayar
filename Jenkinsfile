pipeline {
    properties {
	githubProjectUrl("https://github.com/iliayar/iliayar.git")
    }

    trigger {
	gitHub()
    }

    agent any

    stages {
	stage {
	    steps {
		script {
		    sh "echo AYAYAYAYAYY"
		}
	    }
	}
    }
}
