"""06_surpresa_di.py -- surpresa do Copom no DI futuro, como em BC (2026), seção 4.2.1, p. 43-44.

Para cada reunião ordinária do Copom de fev/2018 a jun/2025 (dia da decisão = t):
  1. baixa da B3 o arquivo diário de preços (BVBG.086, "pesquisa por pregão") de t e do pregão
     seguinte (t+1), guardando os zips em data_b3/ (cerca de 440 MB para as 60 reuniões);
  2. lê, para cada contrato DI1, o PU de ajuste (AdjstdQt) e a taxa de ajuste (AdjstdQtTax);
     os dias úteis até o vencimento saem do próprio par: du = 252 ln(100000/PU) / ln(1 + taxa);
  3. escolhe o contrato T = próximo DI1 com 10 < du < 40 em t;
  4. taxa a termo em t de t+1 a T, tirando o CDI do dia t (SGS 12):
        (1 + r_t)^(du/252) = (1 + DI_t)^(1/252) (1 + f)^((du - 1)/252);
  5. surpresa = r_{t+1}(T) - f, em p.p.; |surpresa| < 0,05 p.p. vira zero.

Antes de 2018 a B3 não publica esses arquivos (testado: 2011, 2015, 2016, 2017 vazios).

Entradas: data/copom_historico.csv (gerado por R/01_dados.R) e data/bcb_12_cdi_diario.csv (CDI
diário do SGS, baixado aqui se faltar).
Saídas: data_b3/tratado/di1_ajustes.csv (todos os DI1 dos dias baixados)
        output/surpresa_di_reuniao.csv
Rodar da pasta do estudo, depois de R/01_dados.R: python -I py/06_surpresa_di.py
"""
import csv
import glob
import io
import math
import os
import re
import shutil
import sys
import tempfile
import time
import urllib.request
import zipfile
from datetime import date, timedelta

ESTUDO = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
DADOS_BCB = os.path.join(ESTUDO, "data")
PASTA_B3 = os.path.join(ESTUDO, "data_b3")
TRATADO = os.path.join(PASTA_B3, "tratado")
INICIO, FIM = date(2018, 1, 1), date(2025, 6, 30)
UA = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"


def baixar(d):
    """Zip da B3 para o dia d, ou None se o dia não tem pregão (arquivo vazio)."""
    nome = f"PR{d:%y%m%d}.zip"
    arq = os.path.join(PASTA_B3, nome)
    if os.path.exists(arq):
        return arq if os.path.getsize(arq) > 1000 else None
    url = f"https://www.b3.com.br/pesquisapregao/download?filelist={nome}"
    for tentativa in range(4):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": UA})
            conteudo = urllib.request.urlopen(req, timeout=120).read()
            break
        except Exception as e:  # rede instável: tenta de novo
            print(f"  {nome}: tentativa {tentativa + 1} falhou ({e})")
            time.sleep(5)
    else:
        raise SystemExit(f"falhou: {url}")
    with open(arq, "wb") as f:
        f.write(conteudo)
    time.sleep(1)
    return arq if len(conteudo) > 1000 else None


def di1_do_zip(arq):
    """{ticker: (pu_ajuste, taxa_ajuste)} dos contratos DI1 do arquivo; extrai num diretório novo."""
    tmp = tempfile.mkdtemp(prefix="b3pr_")
    try:
        pend = [arq]
        xmls = []
        while pend:
            z = pend.pop()
            with zipfile.ZipFile(z) as zf:
                destino = tempfile.mkdtemp(dir=tmp)
                zf.extractall(destino)
            for p in glob.glob(os.path.join(destino, "**", "*"), recursive=True):
                if p.lower().endswith(".zip"):
                    pend.append(p)
                elif p.lower().endswith(".xml"):
                    xmls.append(p)
        out = {}
        for x in sorted(xmls):
            s = io.open(x, encoding="utf-8", errors="replace").read()
            for bloco in re.findall(r"<PricRpt>(.*?)</PricRpt>", s, flags=re.S):
                m = re.search(r"<TckrSymb>(DI1[FGHJKMNQUVXZ]\d\d)</TckrSymb>", bloco)
                if not m:
                    continue
                pu = re.search(r"<AdjstdQt Ccy=\"BRL\">([\d.]+)</AdjstdQt>", bloco)
                tx = re.search(r"<AdjstdQtTax Ccy=\"BRL\">([\d.]+)</AdjstdQtTax>", bloco)
                if pu and tx:
                    out.setdefault(m.group(1), (float(pu.group(1)), float(tx.group(1))))
            if out:
                break  # os XMLs do mesmo dia repetem os DI1; basta o primeiro com dados
        return out
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def du_implicito(pu, taxa):
    return 252 * math.log(100000 / pu) / math.log(1 + taxa / 100)


def cdi_diario():
    """CDI diário (SGS 12, % ao dia); baixa do SGS se o arquivo não existir."""
    arq = os.path.join(DADOS_BCB, "bcb_12_cdi_diario.csv")
    if not os.path.exists(arq):
        import json
        url = ("https://api.bcb.gov.br/dados/serie/bcdata.sgs.12/dados?formato=json"
               f"&dataInicial={INICIO:%d/%m/%Y}&dataFinal={FIM + timedelta(days=10):%d/%m/%Y}")
        req = urllib.request.Request(url, headers={"User-Agent": UA})
        linhas = json.loads(urllib.request.urlopen(req, timeout=120).read().decode("utf-8"))
        with open(arq, "w", newline="", encoding="utf-8") as f:
            w = csv.writer(f); w.writerow(["data", "valor"])
            for r in linhas:
                d, m, a = r["data"].split("/"); w.writerow([f"{a}-{m}-{d}", r["valor"]])
    return {r["data"]: float(r["valor"]) for r in csv.DictReader(open(arq, encoding="utf-8"))}


def main():
    os.makedirs(TRATADO, exist_ok=True)
    cdi = cdi_diario()
    copom = list(csv.DictReader(open(os.path.join(DADOS_BCB, "copom_historico.csv"), encoding="utf-8")))
    datas = sorted({date.fromisoformat(r["data_reuniao"]) for r in copom
                    if r["extraordinaria"] == "FALSE" and INICIO <= date.fromisoformat(r["data_reuniao"]) <= FIM})
    print(f"{len(datas)} reuniões de {datas[0]} a {datas[-1]}")

    linhas_di1, linhas_surp = [], []
    for t in datas:
        zt = baixar(t)
        if zt is None:
            raise SystemExit(f"sem pregão no dia da decisão {t}?")
        t1 = t + timedelta(days=1)
        while (zt1 := baixar(t1)) is None:
            t1 += timedelta(days=1)
        d0, d1 = di1_do_zip(zt), di1_do_zip(zt1)
        for dia, dd in ((t, d0), (t1, d1)):
            for tk, (pu, tx) in dd.items():
                linhas_di1.append([dia.isoformat(), tk, pu, tx, round(du_implicito(pu, tx), 2)])
        cand = sorted((round(du_implicito(*v)), tk) for tk, v in d0.items())
        escolhido = next(((du, tk) for du, tk in cand if 10 < du < 40), None)
        di_t = cdi.get(t.isoformat())
        if escolhido is None or di_t is None or escolhido[1] not in d1:
            linhas_surp.append([t.isoformat(), t1.isoformat(), "", "", "", "", "", "", "", ""])
            print(f"{t}: sem contrato elegível ou sem CDI")
            continue
        du, tk = escolhido
        r_t, r_t1 = d0[tk][1], d1[tk][1]
        di_aa = ((1 + di_t / 100) ** 252 - 1) * 100
        f = (((1 + r_t / 100) ** (du / 252) / (1 + di_aa / 100) ** (1 / 252)) ** (252 / (du - 1)) - 1) * 100
        bruta = r_t1 - f
        surpresa = 0.0 if abs(bruta) < 0.05 else bruta
        linhas_surp.append([t.isoformat(), t1.isoformat(), tk, du, r_t, round(di_aa, 4), round(f, 4), r_t1,
                            round(bruta, 4), round(surpresa, 4)])
        print(f"{t} {tk} du={du:2d} r_t={r_t:.3f} DI={di_aa:.3f} termo={f:.3f} r_t+1={r_t1:.3f} surpresa={bruta:+.3f}")

    with open(os.path.join(TRATADO, "di1_ajustes.csv"), "w", newline="", encoding="utf-8") as fo:
        w = csv.writer(fo); w.writerow(["data", "ticker", "pu_ajuste", "taxa_ajuste", "du"]); w.writerows(linhas_di1)
    os.makedirs(os.path.join(ESTUDO, "output"), exist_ok=True)
    with open(os.path.join(ESTUDO, "output", "surpresa_di_reuniao.csv"), "w", newline="", encoding="utf-8") as fo:
        w = csv.writer(fo)
        w.writerow(["data_reuniao", "data_seguinte", "contrato", "du", "taxa_t", "di_t_aa", "termo_t", "taxa_t1",
                    "surpresa_bruta", "surpresa"])
        w.writerows(linhas_surp)
    nz = sum(1 for l in linhas_surp if l[9] not in ("", 0.0))
    print(f"reuniões: {len(linhas_surp)}; surpresa não nula: {nz}")


if __name__ == "__main__":
    main()
