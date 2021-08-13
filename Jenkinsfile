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

void setBuildStatus(String context, String message, String state) {
    step([
	$class: "GitHubCommitStatusSetter",
	reposSource: [
	    $class: "AnyDefinedRepositorySource"
	],
	contextSource: [
	    $class: "ManuallyEnteredCommitContextSource", context: context
	],
	errorHandlers: [
	    [
		$class: "ChangingBuildStatusErrorHandler", result: "FAILURE"
	    ]
	],
	statusResultSource: [
	    $class: "ConditionalStatusResultSource",
	    results: [
		[
		    $class: "AnyBuildResult",
		    state: state,
		    message: "Build #${currentBuild.number} ${message} in ${currentBuild.durationString}"
		]
	    ]
	]
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
    }
    post {
	always {
	    script {
		if (currentBuild.result == "SUCCESS") {
		    setBuildStatus("Org Publish", "succeed", "SUCCESS")
		} else {
		    setBuildStatus("Org Publish", "failed", "FAILURE")
		}
	    }
	}
    }
}
