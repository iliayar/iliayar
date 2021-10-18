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
	stage("Pull changes") {
	    steps {
		sshagent(["iliayar"]) {
		    sh('git pull --rebase origin master')
		}
	    }
	}
	stage("Publishing") {
	    steps {
		script {
		    sh "git config user.name 'iliayar'"
		    sh "git config user.email 'iliayar3@gmail.com'"
		    sh "git checkout master"

		    sh "docker build org-publish/ -t org-publish"
		    sh "org-publish/run.sh"

		    sh "git add public/notes/blog.org"
		    sh "git diff --quiet && git diff --staged --quiet || git commit -m 'Update blog.org with new IDs'"
		}
	    }
	}
	stage("Pushing IDs") {
	    steps {
		sshagent(["iliayar"]) {
		    sh('git pull --rebase origin master')
		    sh("git push origin master")
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
