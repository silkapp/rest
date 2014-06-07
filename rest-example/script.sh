#!/bin/bash
echo "# User list in XML:"
echo ""
curl -s 'http://localhost:3000/v1.0/user' | xmllint --format -
echo ""
echo "-------------"
echo "# User list in JSON:"
echo ""
curl -s 'http://localhost:3000/v1.0/user?type=json' | python -mjson.tool
echo ""
echo "-------------"
echo "# Post list in XML:"
echo ""
curl -s 'http://localhost:3000/v1.0/post' | xmllint --format -
echo ""
echo ""
echo "-------------"
echo "# User list in FayFormat"
echo ""
curl -s 'http://localhost:3000/v1.0/user?type=fay' | python -mjson.tool
echo ""
echo "-------------"
echo "# Creating post:"
echo ""
curl -s -X POST 'http://localhost:3000/v1.0/post' -H "Content-Type:application/json" -d '{"user":{"name":"adam","password":"1234"},"post":{"title":"Interesting Blog Post","content":"I will finish this later"}}' | python -mjson.tool
echo "-------------"
echo "# Creating post with the same title:"
echo ""
curl -s -X POST 'http://localhost:3000/v1.0/post' -H "Content-Type:application/json" -d '{"user":{"name":"adam","password":"1234"},"post":{"title":"Interesting Blog Post","content":"I will finish this later"}}' | python -mjson.tool
echo "-------------"
echo "# Creating a different post using FayFormat:"
echo ""
curl -s -X POST 'http://localhost:3000/v1.0/post' -H "Content-Type:application/x-fay" -d '{"instance":"UserPost", "user":{"instance":"User", "name":"jcberentsen","password":"1212"},"post":{"instance":"CreatePost", "title":"Mildly Interesting Blog Post","content":"I might finish this"}}'
