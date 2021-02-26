#! /bin/bash

curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -X GET -G http://localhost:3000/filterNews -d 'filterCondition=allContent&content=Советский&page=0'
