void setPendingStatus(String message) {
    step([
	$class: "GitHubSetCommitStatusBuilder",
	contextSource: [
	    $class: "ManuallyEnteredCommitContextSource", context: message
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
	    $class: "ConditionalStatusResultSource",
	    results: [
		[
		    $class: "AnyBuildResult"
		]
	    ]
	]
	// statusResultSource: [
	//     $class: "DefaultStatusResultSource"
	//     // results: [
	//     // 	[
	//     // 	    $class: "AnyBuildResult",
	//     // 	    message: message,
	//     // 	    state: state
	//     // 	]
	//     // ]
	// ]
    ]);
}

pipeline {
    agent any

    triggers {
	githubPush()
    }

    stages {
	stage("Publishing") {
	    steps {
		script {
		    setPendingStatus("Org Publish")
		    sh "echo AYAYAYAYAYY"
		}
	    }
	}
	stage("Commit Status") {
	    steps {
		script {
		    setBuildStatus("Org Publish")
		}
	    }
	}
    }
}
