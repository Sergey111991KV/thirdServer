#! /bin/bash



curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -X GET -G http://localhost:3000/filterNews -d 'page=0&filterCondition=oneOfTag&tags_arr=1,2'