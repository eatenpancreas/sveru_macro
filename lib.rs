use proc_macro::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{ItemFn, LitStr, parse, parse_macro_input};

struct Endpoint {
  route: syn::LitStr,
  method: syn::LitStr,
  in_t: syn::Type,
  out_t: syn::Type,
  query_t: syn::Type,
}

impl parse::Parse for Endpoint {
  fn parse(input: parse::ParseStream) -> Result<Self, syn::Error> {
    let route = input.parse()?;
    let _ = input.parse::<syn::Token![,]>();
    let method = input.parse()?;
    let _ = input.parse::<syn::Token![,]>();
    let in_t = input.parse()?;
    let _ = input.parse::<syn::Token![,]>();
    let out_t = input.parse()?;
    let _ = input.parse::<syn::Token![,]>();
    let query_t = input.parse()?;

    Ok(Endpoint {
      route, method,
      in_t, out_t, query_t
    })
  }
}

#[proc_macro_attribute]
pub fn endpoint(ep: TokenStream, func: TokenStream) -> TokenStream {
  let ep = parse_macro_input!(ep as Endpoint);
  let func = parse_macro_input!(func as ItemFn);

  let struct_name = format_ident!("ExportEndpoint{}",
    capitalize(func.sig.ident.to_string()));
  let test_name = format_ident!("export_write_endpoint_{}", func.sig.ident);

  let route = ep.route.value();
  let method_lower = ep.method.value().to_lowercase();
  let method_upper = method_lower.to_uppercase();
  let struct_name_str = struct_name.to_string();
  let in_t = ep.in_t;
  let out_t = ep.out_t;
  let query_t = ep.query_t;

  let route_raw = format!("../bindings/{}/{}.ts", route, method_upper);
  let route_literal = LitStr::new(route_raw.as_str(), Span::call_site().into());
  let expanded = quote! {
    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize, ts_rs::TS)]
    #[ts(export, export_to = #route_literal)]
    struct #struct_name {
      in_type: #in_t,
      out_data_type: #out_t,
      query_type: #query_t
    }

    #[cfg(test)]
    #[test]
    fn #test_name() {
      sveru::macro_functions::write_endpoint(#method_lower, #method_upper, #route, #struct_name_str);
    }
  };

  TokenStream::from(quote!(
    #expanded
    #func
  ))
}

fn capitalize(string: String) -> String {
  let mut new_string = String::new();

  let mut next_is_capitalized = true;
  for c in string.chars() {
    if c.is_alphabetic() && next_is_capitalized {
      next_is_capitalized = false;
      for c_upper in c.to_uppercase() {
        new_string.push(c_upper);
      }
    } else if c == '_' {
      next_is_capitalized = true;
    } else {
      next_is_capitalized = false;
      new_string.push(c);
    }
  }

  new_string
}
