local function ok()
 RunConsoleCommand("say", "HI") -- change Hi to what ever you want to say
end

timer.Create("what", 0.01, 0, ok) 