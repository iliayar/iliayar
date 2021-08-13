void setPendingStatus() {
    step([
	$class: "GitHubSetCommitStatusBuilder",
	contextSource: [
	    $class: "DefaultCommitContextSource"
	],
	statusMessage: [
	    content: "Publishing Org files"
	]
    ]);
}

void setBuildStatus(String message /*, String state */) {
    step([
	$class: "GitHubCommitStatusSetter",
	reposSource: [
	    $class: "AnyDefinedRepositorySource"
	],
	contextSource: [
	    $class: "ManuallyEnteredCommitContextSource", context: message
	],
	errorHandlers: [
	    [
		$class: "ChangingBuildStatusErrorHandler", result: "UNSTABLE"
	    ]
	],
	statusResultSource: [
	    $class: "DefaultStatusResultSource"
	    // results: [
	    // 	[
	    // 	    $class: "AnyBuildResult",
	    // 	    message: message,
	    // 	    state: state
	    // 	]
	    // ]
	]
    ]);
}

pipeline {
    agent any

    triggers {
	githubPush()
    }

    stages {
	stage("Test") {
	    steps {
		script {
		    setPendingStatus()
		    sh "echo AYAYAYAYAYY"
		    setBuildStatus("Org Publish")
		}
	    }
	}
    }
}
