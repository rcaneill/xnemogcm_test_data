var compile_dict={"-1": "&#10060;", "1": "&#9989;"}
function generateCompileTable()
{
  var machine_list=[];
  var machine_revision_list=[];
  var machine_full_name_list=[];
  var machine_info_list=[];

  if(typeof jeanzay_revision_list != 'undefined') 
  {
    machine_list.push("jeanzay");
    machine_revision_list.push("jeanzay_revision_list")
    machine_full_name_list.push("Jean-Zay")
    machine_info_list.push("jeanzay_compile_info_list")
  }
  
  if (typeof irene_revision_list != 'undefined') 
  {
    machine_list.push("irene");
    machine_revision_list.push("irene_revision_list")
    machine_full_name_list.push("Irene")
    machine_info_list.push("irene_compile_info_list")
  }
  
  if (typeof other_revision_list != 'undefined') 
  {
    machine_list.push("other");
    machine_revision_list.push("other_revision_list")
    machine_full_name_list.push("Other")
    machine_info_list.push("other_compile_info_list")
  }

  var revision_list=[]

  for (var i=0; i<machine_list.length; i++)
  {
    tmp_list = eval(machine_revision_list[i])

    for (var j=0; j<tmp_list.length; j++)
    {
      if (revision_list.includes(tmp_list[j]) == false)
      {
        revision_list.push(tmp_list[j])
      }
    }
  }

  revision_list.sort(function(a, b){return b-a});
  
  
  // Generate select list for compile table

  var sel = document.getElementById("revision");

  var sel_options = sel.children
  
  var new_options=[]
  for(var i=0; i<sel_options.length-1; i++)
  {
    new_options[i] = sel_options[i+1].value
  }
  
  for(var i=0; i<revision_list.length; i++)
  {
    if (!new_options.includes(revision_list[i]))
    {
      new_options.push(revision_list[i])
    }
  }
  
  new_options.sort(function(a, b){return b-a})
  
  while (sel.lastChild && sel.children.length>1) {
    sel.removeChild(sel.lastChild);
  }

  for(var i=0; i<new_options.length; i++)
  {
    var opt = document.createElement('option');
    opt.appendChild( document.createTextNode(new_options[i]));
    opt.value = new_options[i]; 
    sel.appendChild(opt);
  }
  
  // Generate content of compile table
  
  
  var table = document.getElementById("compile_table")
  table.style.backgroundColor = "#84c5ff";
  table.setAttribute("class", "compile_table")
  table.setAttribute("id", "compile_table")

  var titelRow = document.getElementById("compile_table_row0")
  var myCell = document.createElement("TD");
  myCell.style.maxWidth="100px"
  myCell.innerHTML = ("Revision");
  titelRow.appendChild(myCell)

  for (var machine_index=0; machine_index<machine_list.length; machine_index++)
  {
    var myCell = document.createElement("TD");
    myCell.style.minWidth="500px"
    myCell.innerHTML = (machine_full_name_list[machine_index]);
    titelRow.appendChild(myCell)
  }

  var emptyRow = document.createElement("tr")
  emptyRow.setAttribute("id", "empty_compile_row")
  emptyRow.style.display="none"
  table.appendChild(emptyRow)
  
  var myCell = document.createElement("TD");
  myCell.setAttribute("id", "empty_compile_row_revision")
  myCell.style.fontWeight="bold"
  emptyRow.appendChild(myCell)

  for (var machine_index=0; machine_index<machine_list.length; machine_index++)
  {
    var myCell = document.createElement("TD");
    myCell.innerHTML="No Compile Information"
    myCell.style.color="white"
    emptyRow.appendChild(myCell)
  }

  for (var i=0; i<revision_list.length; i++)
  {
    var revision = revision_list[i]

    var myRow = document.createElement("TR");
    myRow.setAttribute("id", "compile_table_"+revision);
    myRow.setAttribute("class", "compile_table_row");
    table.appendChild(myRow)
    
    var myCell = document.createElement("TD");
    myCell.innerHTML = (revision);
    myCell.style.fontWeight = "bold";
    myRow.appendChild(myCell)
    
    for( var j=0; j<machine_list.length; j++)
    {
      var machine = machine_list[j]
      var myCell = document.createElement("TD");
      myCell.setAttribute("id", "compile_table_"+revision+"_"+machine);
      myRow.appendChild(myCell)
    }        

  }

  for (var i=0; i<revision_list.length; i++)
  {
    var revision = revision_list[i]
    for ( var j=0; j<machine_list.length; j++)
    {
      var machine = machine_list[j]
      fillSubCompileTable(revision, machine, machine_info_list[j], machine_revision_list[j])
    }
  }

}



function fillSubCompileTable(revision, machine, machine_compile_info, machine_revision)
{
  var compile_info = eval(machine_compile_info)
  var revision_list = eval(machine_revision)

  var myTD = document.getElementById("compile_table_"+revision+"_"+machine);
  if (!revision_list.includes(revision))
  {
    myTD.innerHTML="No Compile Information"
    myTD.style.color="white"
  }
  else
  {
    var part_compile_info = []
    var tmp_mode_list = []
    var tmp_arch_list = []
    for(var i=0; i<compile_info.length; i++)
    {
      if (revision == compile_info[i][0])
      {
        part_compile_info.push(compile_info[i])
        if (!tmp_mode_list.includes(compile_info[i][3]))
        {
          tmp_mode_list.push(compile_info[i][3])
        }
        if (!tmp_arch_list.includes(compile_info[i][2]))
        {
          tmp_arch_list.push(compile_info[i][2])
        }
      }
    }

    mySubTable = document.createElement("table")
    mySubTable.setAttribute("class", "compile_sub_table")
    myTD.appendChild(mySubTable)

    myRow = document.createElement("tr")
    mySubTable.appendChild(myRow)
    myCell = document.createElement("td")
    myRow.appendChild(myCell)
    for (var j=0; j<tmp_mode_list.length; j++)
    {
      myCell = document.createElement("td")
      myCell.innerHTML=(tmp_mode_list[j])
      myRow.appendChild(myCell)
    }

    for (var j=0; j<tmp_arch_list.length; j++)
    {
      myRow = document.createElement("tr")
      mySubTable.appendChild(myRow)

      myCell = document.createElement("td")
      myCell.innerHTML = (tmp_arch_list[j])
      myRow.appendChild(myCell)

      
      for(var i=0; i<tmp_mode_list.length; i++)
      {
        myCell = document.createElement("td")
        for (var k=0; k<part_compile_info.length; k++)
        {
          if(part_compile_info[k][2] == tmp_arch_list[j] && part_compile_info[k][3] == tmp_mode_list[i])
          {
            myCell.innerHTML = compile_dict[part_compile_info[k][4]]
            break
          }
        }        
        myRow.appendChild(myCell)
      }
    }
  }

  
}
