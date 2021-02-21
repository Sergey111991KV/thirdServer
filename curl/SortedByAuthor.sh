#! /bin/bash



curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -X GET -G http://localhost:3000/sortedNews -d 'page=0&conditionSorted=author'
