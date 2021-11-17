/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';


const features = [
  {
    title: 'Omni-Channel Efficiency with Hudsons Bay Company',
    imageUrl: 'https://upload.wikimedia.org/wikipedia/commons/thumb/0/04/Hudson%27s_Bay_Company_Official_Logo_2013.svg/1280px-Hudson%27s_Bay_Company_Official_Logo_2013.svg.png',
    description: (
      <>
        Using GeoLift, Hudson’s Bay was able to measure lift in online and in-store sales
        as a result of campaigns on Facebook and Instagram. The company saw a 12.9X incremental
        return on ad spend (iROAS) utilizing this approach.
        <a href="https://www.facebook.com/business/measurement/case-studies/hudsons-bay-company#">
        -  Learn More.
        </a>
      </>
    ),
  },
  {
    title: 'Omni-Channel Measurement for Scotiabank',
    imageUrl: 'https://i.pinimg.com/originals/3f/9c/83/3f9c8390a12bf2b96c7055169e77f333.png',
    description: (
      <>
        Scotiabank Mexico was able to measure for the first time the omni-channel
        incremental impact that their Facebook campaigns had on bank account openings.
        <a href="https://www.facebook.com/business/success/2-scotiabank-mexico">
        -  Learn More.
        </a>
      </>
    ),
  },
  {
    title: 'Building Brand and Sales with Super Coffee',
    imageUrl: 'https://scontent.fmex11-1.fna.fbcdn.net/v/t1.6435-1/p100x100/146475455_3695451970571166_7309961661612546269_n.png?_nc_cat=105&ccb=1-5&_nc_sid=dbb9e7&_nc_eui2=AeG9H3ydn7q54QvS6IsuGgB7NEmGhia1Teo0SYaGJrVN6oOiRbUV_8TDSBfl_ngJ_Bk&_nc_ohc=bGs-EXUgaOAAX-PiICp&_nc_ht=scontent.fmex11-1.fna&oh=3d09c99325629e374294277c187c7a73&oe=61BA2B3A',
    description: (
      <>
        The enhanced coffee company ran Facebook video and photo ads to boost brand
        awareness and used a GeoLift study to measure in-store sales, revealing an
        11.9% in-store sales lift.
        <a href="hhttps://www.facebook.com/business/success/super-coffee">
        -  Learn More.
        </a>
      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

export default function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`Hello from ${siteConfig.title}`}
      description="Description will go into a meta tag in <head />">
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <h1 className="hero__title">{"GeoLift Success Cases"}</h1>
          <div className={styles.buttons}>
          </div>
        </div>
      </header>
      <main>
        {features && features.length > 0 && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {features.map(({title, imageUrl, description}) => (
                  <Feature
                    key={title}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}
      </main>
    </Layout>
  );
}
