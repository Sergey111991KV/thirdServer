#! /bin/bash



curl -H application/x-www-form-urlencoded -b 'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -X GET -G http://localhost:3000/filterNews -d 'page=0&filterCondition=author&author_id=2'