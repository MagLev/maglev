# A bare-bones app to do Gprof testing of MagLev w/o Sinatra in the way.
#
# Usage: maglev-ruby rack_app.rb

require 'rack'

app = Proc.new do |env|
  case env['REQUEST_PATH']

  when '/start'
    puts "START"
    ns_per_sample = Maglev::Gprof.compute_interval(10)  # figure out for 10 seconds
    puts "------- ns_per_sample: #{ns_per_sample}"
    $monitor = Maglev::Gprof.create(ns_per_sample)
    $monitor.resume_sampling

  when '/stop'
    puts "STOP"
    $monitor.suspend_sampling

  when '/report'
    puts "REPORT"
    puts $monitor.stop_and_report
    file_name = 'gprof.out'
    puts "Writing report to #{file_name}."
    puts "  temp_obj_space_percent_used: #{System.temp_obj_space_percent_used}"
    puts "  temp_obj_space_used: #{System.temp_obj_space_used}"
    report = $monitor.stop_and_report
    puts report
    File.delete file_name if File.exist? file_name
    File.open(file_name, File::WRONLY|File::CREAT) do |file|
      file << report
    end
    puts "Done writing report."
    $monitor = nil
  end

  [200, { 'Content-Type' => 'text/html' }, "<h1>hello</h2>" ]
end

Rack::Handler::SCGI.run(app, :Host=> 'localhost', :Port => '4567')

