if (...) <= 1 then 
	return 1
else
  	return (...) * (load("return fat(" .. ((...)-1) .. ")"))()
end
