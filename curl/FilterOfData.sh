#! /bin/bash



curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -X GET -G http://localhost:3000/filterNews -d 'page=0&filterCondition=date&condition=less&date=2011-08-01'