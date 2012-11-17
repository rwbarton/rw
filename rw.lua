local counter = 0
function ready()
   counter = counter + 1
   crawl.mpr("Lambda " .. counter)
end
