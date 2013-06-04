y = Thread.start do
  begin
    begin
      1 / 0
    ensure
      0
    end
  rescue Exception => e
    Thread.stop
  end
end

class Thread
  primitive '__local_frame_contents_at', '_localFrameContentsAt:'
  primitive '__local_stack_depth', 'localStackDepth'
  primitive '__local_method_at', 'localMethodAt:'
end

a = y.__local_frame_contents_at(9)[9].instance_variable_get("@_st__method")
raise "instvar getter not overridden for ExecBlock" unless a.class == GsNMethod
# a.class should not be GsNativeCode or a.inspect will segfault

