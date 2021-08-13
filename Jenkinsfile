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
	stage("Set pending status") {
	    steps {
		script {
		    setPendingStatus("Org Publish")
		}
	    }
	}
	stage("Publishing") {
	    steps {
		script {
		    sh "git checkout master"

		    sh "cd org-publish"
		    sh "docker build . -t org-publish"
		    sh "./run.sh"
		    sh "cd ../"

		    sh "git add public/notes/blog.org"
		    sh "git diff --quiet && git diff --staged --quiet || git commit -m 'Update blog.org with new IDs'"
		}
	    }
	}
	stage("Pushing IDs") {
	    sshagent(["iliayar"]) {
		sh("git push origin master")
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
