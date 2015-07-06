Given(/^the code$/) do |code|
  @code = code
  File.open('cucumber.tmp.ks', 'w') do |f|
    f.write(@code)
    f.close
  end
end

Then(/^it should indent like$/) do |example|
  emacs = 'emacs --batch --no-init-file --load ks.el --eval \'(find-file "cucumber.tmp.ks")\' --eval "(ks-mode)" --eval "(ks-indent-buffer)" --eval \'(save-buffer)\''
  system emacs
  indented_code = File.open('cucumber.tmp.ks').read
  if indented_code != example
#    puts '---- Result ----'
#    puts indented_code
#    puts '----------------'
    raise  "But we got\n\"\"\"\n" + indented_code + "\n\"\"\"\n"
  end
end
