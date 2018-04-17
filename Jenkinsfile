def version = env.BRANCH_NAME

pipeline {
    agent { label 'slave-sbt' }
    parameters {
        booleanParam(defaultValue: false, description: 'Run integration tests', name: 'run')
    }
    stages {
        stage("Review") {
            when {
                expression { env.CHANGE_ID != null }
            }
            steps {
                checkout scm
                sh 'sbt clean scalafmtCheck scalafmtSbtCheck test:scalafmtCheck scapegoat test:scapegoat test:compile'
            }
        }
        stage("Run") {
            when {
                expression { params.run }
            }
            steps {
                checkout scm
                sh 'sbt test'
            }
            post {
                always {
                    junit '**/target/test-reports/*.xml'
                }
            }
        }
    }
}
