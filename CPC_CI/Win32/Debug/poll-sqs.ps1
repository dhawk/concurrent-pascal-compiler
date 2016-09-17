$resp=Receive-SQSMessage -QueueUrl https://sqs.us-west-2.amazonaws.com/388134025491/github-cpc-push-notification
if($resp)
{ 
   Remove-SQSMessage -QueueUrl https://sqs.us-west-2.amazonaws.com/388134025491/github-cpc-push-notification -ReceiptHandle $resp.ReceiptHandle -Force
}
$resp.Body
