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
		    message: message
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
		    sh "false"
		}
	    }
	}
	post {
	    always {
		if (currentBuild.result == "SUCCESS") {
		    script {
			setBuildStatus("Org Publish", "Build #${currentBuild.number} successful in ${currentBuild.duration}", "SUCCESS")
		    }
		} else {
		    script {
			setBuildStatus("Org Publish", "Build #${currentBuild.number} failed in ${currentBuild.duration}", "FAILURE")
		    }
		}
	    }
	}
    }
}
