return {
  {
    Str = function (elem)
       if elem.text == "{{raw}}" then
          if FORMAT == "gfm" then
             return pandoc.Str "{% raw %}"
          else
             return pandoc.Str ""
          end
       elseif elem.text == "{{endraw}}" then
          if FORMAT == "gfm" then
             return pandoc.Str "{% endraw %}"
          else
             return pandoc.Str ""
          end
       else
          return elem
       end
    end,
  }
}
