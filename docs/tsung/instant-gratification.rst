Tsung - Instant gratification
=============================

Tsung is a tool for load testing web applications and databases.

The tsung command that we will look into now are

1. tsung-recorder, is a tool that can act as a proxy and record
network track and record network requests to a live client. These
recordings can be played back, on a much larger scale, by the real tsung
program.

2. tsung, which is the actual program that reads from a configuration file
that includes one or more "sessions". These sessions can be ones that are 
created by the tsung-recorder, or handcoded.

3. tsung-stats.pl, is a tool that will generate html reports with graphs
that presents all the metrics collected by the test session.

Tsung recorder
--------------

Tsung recorder is started so,

.. code-block:: bash

  tsung-recorder start

It will print out something like 

 "Record file: /home/vagrant/.tsung/tsung_recorder20161130-0627.xml"

Which means that the recorded sessions will be stored at that path.

The proxy will be listening on port 8090. So now you can go change
the proxy settings in your browser and start using the app.

After you are done, stop the recording using

.. code-block:: sh

  tsung-stop

Tsung
-----

The tsung uses a configuration file to do the test. Let us start with
the very basic test. Let us test couple of json end points.

.. code-block:: xml

  <?xml version="1.0"?>
     <!DOCTYPE tsung SYSTEM "/opt/local/share/tsung/tsung-1.0.dtd">
     <tsung loglevel="warning">
     <clients>
        <client host="localhost" cpu="1" maxusers="3000" use_controller_vm="true"/>
     </clients>
     <servers>
        <server host="192.168.33.5" port="8080" type="tcp"/>
     </servers>
     <load>
        <arrivalphase phase="1" duration="1" unit="minute">
           <users arrivalrate="5" unit="second"/>
        </arrivalphase>
      </load>
     <sessions>
        <session name='rec20161129-1447' probability='100'  type='ts_http'>
        <setdynvars sourcetype="random_string" length="20">
          <var name="booffice"/>
        </setdynvars>
        <request><http url='/tenants' version='1.1' method='GET'></http></request>

        <thinktime random='true' value='32'/>

        <request subst="true"><http url='/tenants/new' version='1.1'  contents="{&quot;id&quot;: 532, &quot;name&quot;: &quot;Tewvrewer&quot;, &quot;firstname&quot;: &quot;TenantwerweFtern&quot;, &quot;lastname&quot; : &quot;TenaewrwntLn&quot;, &quot;email&quot;: &quot;TenantEweqweqerwrml2&quot;, &quot;phone&quot;: &quot;TenawerwerntPhone&quot;,  &quot;userId&quot;: null, &quot;backofficeDomain&quot;: &quot;%%_booffice%%&quot;}" content_type='application/json; charset=utf-8' method='POST'></http></request>
        </session>
     </sessions>
   </tsung>

Let us take a look at relavent sections. 

.. code-block:: xml
  <client host="localhost" cpu="1" maxusers="3000" use_controller_vm="true"/>

1. The host attribute of localhost means that we need the requests to be sent from this machine itself.
2. The *use_controller_vm* attribute means that you don't want to tsung to connect to
   another machine, and run tests from there, and instead run the test from the parent process itself.
   if you omit this attribute, tsung will try to ssh into *localmachine* and will often result in permission
   denied errors. 

.. code-block:: xml

     <servers>
        <server host="192.168.33.5" port="8080" type="tcp"/>
     </servers>

The host and port are pretty self explanatory.

.. code-block:: xml

        <setdynvars sourcetype="random_string" length="20">
          <var name="booffice"/>
        </setdynvars>

This is where we set up a string which will get randomly generated for every request. We are using this variable here, which, by the way
represents a single request.

.. code-block:: xml

        <request subst="true"><http url='/tenants/new' version='1.1'  contents="{&quot;id&quot;: 532, &quot;name&quot;: &quot;Tewvrewer&quot;, &quot;firstname&quot;: &quot;TenantwerweFtern&quot;, &quot;lastname&quot; : &quot;TenaewrwntLn&quot;, &quot;email&quot;: &quot;TenantEweqweqerwrml2&quot;, &quot;phone&quot;: &quot;TenawerwerntPhone&quot;,  &quot;userId&quot;: null, &quot;backofficeDomain&quot;: &quot;%%_booffice%%&quot;}" content_type='application/json; charset=utf-8' method='POST'></http></request>

Here, the *subst* attribute is important. Without it, variable substitutions will not happen. Take a look at the content attribute. You can see
that it is a double quoted string, which is required for variable substitution to happen. You can see that we are using the *booffice* variable as
*%%_booffice%%*.

tsung-stats.pl
--------------

This is a perl script that can genenrate html reports with a lot of graphs
and stuff. This is how you use it.

.. code-block:: sh

  /usr/lib/tsung/bin/tsung_stats.pl --dygraph -stats ~/.tsung/log/20161129-1705/tsung.log chromium graph.html
