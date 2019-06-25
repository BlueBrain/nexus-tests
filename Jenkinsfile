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
                sh 'sbt clean scalafmtCheck scalafmtSbtCheck test:scalafmtCheck scapegoat test:scapegoat'
            }
        }
        stage("Run") {
            environment {
                SERVICE_ACCOUNT_TOKEN = sh(script: "oc get secret nexus-sa -o jsonpath='{.data.service-account-token}' --namespace=bbp-nexus-dev | base64 --decode", returnStdout: true).trim()
                TEST_USER_TOKEN = sh(script: "oc get secret nexus-test-user -o jsonpath='{.data.test-user-token}' --namespace=bbp-nexus-dev | base64 --decode", returnStdout: true).trim()
                TEST_REALM = sh(script: "oc get secret nexus-test-user -o jsonpath='{.data.test-user-realm}' --namespace=bbp-nexus-dev | base64 --decode", returnStdout: true).trim()
                S3_ACCESS_KEY = sh(script: "oc get secret minio -o jsonpath='{.data.access-key}' --namespace=bbp-nexus-dev | base64 --decode", returnStdout: true).trim()
                S3_SECRET_KEY = sh(script: "oc get secret minio -o jsonpath='{.data.secret-key}' --namespace=bbp-nexus-dev | base64 --decode", returnStdout: true).trim()
            }
            when {
                expression { params.run }
            }
            steps {
                checkout scm
                sh 'sbt test'
            }
            post {
                always {
                    junit 'target/test-reports/TEST*.xml'
                }
            }
        }
    }
}
