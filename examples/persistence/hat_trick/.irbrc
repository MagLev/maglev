# Set prompts so we can easily distinguish between the magician and
# assistant VMs during the IRB hat trick.
#
# Start maglev-irb like:
#    maglev-irb --prompt magician
#    maglev-irb --prompt assistant
#
IRB.conf[:PROMPT][:MAGICIAN] =  {
  :RETURN=>"=> %s\n",
  :PROMPT_I=>"Magician :%03n:%i> ",
  :PROMPT_N=>"Magician :%03n:%i> ",
  :PROMPT_S=>"Magician :%03n:%i%l ",
  :PROMPT_C=>"Magician :%03n:%i* "
}
IRB.conf[:PROMPT][:ASSISTANT] =  {
  :RETURN=>"=> %s\n",
  :PROMPT_I=>"Assistant :%03n:%i> ",
  :PROMPT_N=>"Assistant :%03n:%i> ",
  :PROMPT_S=>"Assistant :%03n:%i%l ",
  :PROMPT_C=>"Assistant :%03n:%i* "
}
